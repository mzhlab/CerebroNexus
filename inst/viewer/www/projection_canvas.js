// Lightweight shared renderer for 2-D cell projections.
(function () {
  'use strict';

  const plots = new Map();
  const PAD = 26;
  const palettes = {
    'Cerebro orange': [[0, '#aeb5bb'], [0.08, '#f7c89d'], [0.38, '#f49a4c'], [0.7, '#e75f25'], [1, '#9f251f']],
    YlGnBu: [[0, '#ffffd9'], [.25, '#c7e9b4'], [.5, '#7fcdbb'], [.75, '#2c7fb8'], [1, '#253494']],
    YlOrRd: [[0, '#ffffcc'], [.25, '#fed976'], [.5, '#fd8d3c'], [.75, '#e31a1c'], [1, '#800026']],
    Blues: [[0, '#f7fbff'], [.25, '#deebf7'], [.5, '#9ecae1'], [.75, '#4292c6'], [1, '#084594']],
    Greens: [[0, '#f7fcf5'], [.25, '#e5f5e0'], [.5, '#a1d99b'], [.75, '#31a354'], [1, '#006d2c']],
    Reds: [[0, '#fff5f0'], [.25, '#fee0d2'], [.5, '#fc9272'], [.75, '#de2d26'], [1, '#a50f15']],
    RdBu: [[0, '#67001f'], [.25, '#d6604d'], [.5, '#f7f7f7'], [.75, '#4393c3'], [1, '#053061']],
    Viridis: [[0, '#440154'], [.25, '#3b528b'], [.5, '#21918c'], [.75, '#5ec962'], [1, '#fde725']]
  };

  function finite(v) { return Number.isFinite(Number(v)); }
  function extent(values) {
    let lo = Infinity, hi = -Infinity;
    values.forEach(function (v) {
      v = Number(v);
      if (!Number.isFinite(v)) return;
      if (v < lo) lo = v;
      if (v > hi) hi = v;
    });
    if (!Number.isFinite(lo)) return [0, 1];
    if (lo === hi) return [lo - .5, hi + .5];
    return [lo, hi];
  }
  function paddedBounds(x, y) {
    const xb = extent(x), yb = extent(y);
    const px = (xb[1] - xb[0]) * .05, py = (yb[1] - yb[0]) * .05;
    return { x0: xb[0] - px, x1: xb[1] + px, y0: yb[0] - py, y1: yb[1] + py };
  }
  function hexRgb(hex) {
    const s = String(hex || '#888').replace('#', '');
    const n = parseInt(s.length === 3 ? s.replace(/(.)/g, '$1$1') : s, 16);
    return [n >> 16 & 255, n >> 8 & 255, n & 255];
  }
  function colorAt(scale, t) {
    scale = Array.isArray(scale) ? scale : (palettes[scale] || palettes.Viridis);
    t = Math.max(0, Math.min(1, t));
    let a = scale[0], b = scale[scale.length - 1];
    for (let i = 1; i < scale.length; i++) {
      if (t <= Number(scale[i][0])) { a = scale[i - 1]; b = scale[i]; break; }
    }
    const span = Number(b[0]) - Number(a[0]) || 1;
    const u = (t - Number(a[0])) / span;
    const ac = hexRgb(a[1]), bc = hexRgb(b[1]);
    return 'rgb(' + ac.map(function (v, i) { return Math.round(v + (bc[i] - v) * u); }).join(',') + ')';
  }
  function cleanText(html) {
    const div = document.createElement('div');
    div.innerHTML = html || '';
    return div.textContent || '';
  }
  function key(x, y) { return String(x) + '-' + String(y); }

  function hostFor(id) { return document.getElementById(id); }
  function ensure(id) {
    let state = plots.get(id);
    const host = hostFor(id);
    if (!host) return null;
    if (state && state.host === host) return state;
    if (state) destroy(state);
    if (host._fullLayout && window.Plotly && Plotly.purge) Plotly.purge(host);
    host.innerHTML = '';
    host.classList.add('cerebro-canvas-host');
    const canvas = document.createElement('canvas');
    canvas.className = 'cerebro-projection-canvas';
    canvas.setAttribute('aria-label', 'Interactive cell projection');
    canvas.tabIndex = 0;
    const tip = document.createElement('div');
    tip.className = 'cerebro-canvas-tip';
    const select = document.createElement('div');
    select.className = 'cerebro-canvas-selection';
    const mini = document.createElement('canvas');
    mini.className = 'cerebro-canvas-minimap';
    mini.width = 84;
    mini.height = 84;
    const toolbar = document.createElement('div');
    toolbar.className = 'cerebro-canvas-toolbar';
    toolbar.innerHTML = '<button type="button" class="cerebro-plot-tool" data-mode="select" title="Box select" aria-label="Box select"><i class="fas fa-vector-square"></i></button><button type="button" class="cerebro-plot-tool is-active" data-mode="lasso" title="Lasso select" aria-label="Lasso select"><i class="fas fa-draw-polygon"></i></button><button type="button" class="cerebro-plot-tool" data-mode="pan" title="Pan" aria-label="Pan"><i class="fas fa-up-down-left-right"></i></button><button type="button" class="cerebro-plot-tool" data-action="zoom-in" title="Zoom in" aria-label="Zoom in"><i class="fas fa-search-plus"></i></button><button type="button" class="cerebro-plot-tool" data-action="zoom-out" title="Zoom out" aria-label="Zoom out"><i class="fas fa-search-minus"></i></button><button type="button" class="cerebro-plot-tool" data-action="reset" title="Reset view" aria-label="Reset view"><i class="fas fa-home"></i></button><button type="button" class="cerebro-plot-tool" data-action="download" title="Download PNG" aria-label="Download PNG"><i class="fas fa-download"></i></button>';
    host.appendChild(canvas); host.appendChild(select); host.appendChild(mini); host.appendChild(tip); host.appendChild(toolbar);
    state = {
      id: id, host: host, canvas: canvas, ctx: canvas.getContext('2d'), tip: tip,
      select: select, mini: mini, miniCtx: mini.getContext('2d'), points: [], centers: [], shapes: [], selected: new Set(),
      hidden: new Set(), bounds: null, fullBounds: null, drag: null, zoomed: false,
      radius: 2.5, opacity: .85, raf: null, mode: 'lasso', toolbar: toolbar
    };
    plots.set(id, state);
    bind(state);
    state.observer = typeof ResizeObserver === 'function' ? new ResizeObserver(function () { resize(state); }) : null;
    if (state.observer) state.observer.observe(host);
    resize(state);
    const gate = host.closest('.cerebro-projection-gate');
    if (gate) gate.classList.add('is-sized');
    return state;
  }
  function destroy(state) {
    if (state.observer) state.observer.disconnect();
    if (state.raf) cancelAnimationFrame(state.raf);
  }
  function deactivate(id) {
    const state = plots.get(id);
    if (!state) return;
    destroy(state);
    plots.delete(id);
    state.host.classList.remove('cerebro-canvas-host');
    state.host.innerHTML = '';
  }
  function resize(state) {
    const rect = state.host.getBoundingClientRect();
    if (!rect.width || !rect.height) return;
    const dpr = Math.min(2, window.devicePixelRatio || 1);
    const w = Math.round(rect.width), h = Math.round(rect.height);
    if (state.canvas.width !== Math.round(w * dpr) || state.canvas.height !== Math.round(h * dpr)) {
      state.canvas.width = Math.round(w * dpr); state.canvas.height = Math.round(h * dpr);
      state.canvas.style.width = w + 'px'; state.canvas.style.height = h + 'px';
      state.ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    }
    state.mini.width = 84 * dpr;
    state.mini.height = 84 * dpr;
    state.miniCtx.setTransform(dpr, 0, 0, dpr, 0, 0);
    state.width = w; state.height = h; schedule(state);
  }
  function schedule(state) {
    if (state.raf) return;
    state.raf = requestAnimationFrame(function () { state.raf = null; draw(state); });
  }
  function screen(state, x, y) {
    const b = state.bounds || state.fullBounds || {x0:0,x1:1,y0:0,y1:1};
    return [PAD + (x - b.x0) / (b.x1 - b.x0) * (state.width - PAD * 2),
      state.height - PAD - (y - b.y0) / (b.y1 - b.y0) * (state.height - PAD * 2)];
  }
  function dataAt(state, sx, sy) {
    const b = state.bounds;
    return [b.x0 + (sx - PAD) / (state.width - PAD * 2) * (b.x1 - b.x0),
      b.y0 + (state.height - PAD - sy) / (state.height - PAD * 2) * (b.y1 - b.y0)];
  }
  function selectedBounds(state) {
    const xs = [], ys = [];
    state.points.forEach(function (p) {
      if (state.selected.has(p.key) && !state.hidden.has(p.group)) {
        xs.push(p.x); ys.push(p.y);
      }
    });
    return xs.length ? paddedBounds(xs, ys) : null;
  }
  function drawMinimap(state) {
    const ctx = state.miniCtx, size = 84, full = state.fullBounds;
    const show = state.selected.size > 0 || state.zoomed;
    state.mini.style.display = show ? 'block' : 'none';
    if (!show || !full) return;
    ctx.clearRect(0, 0, size, size);
    ctx.fillStyle = 'rgba(255,255,255,.92)'; ctx.fillRect(0, 0, size, size);
    const map = function (x, y) {
      return [
        5 + (x - full.x0) / (full.x1 - full.x0) * 74,
        79 - (y - full.y0) / (full.y1 - full.y0) * 74
      ];
    };
    state.points.forEach(function (p) {
      if (state.hidden.has(p.group)) return;
      const q = map(p.x, p.y);
      ctx.fillStyle = state.selected.has(p.key) ? '#2563b8' : '#b8c0c8';
      ctx.globalAlpha = state.selected.has(p.key) ? .95 : .45;
      ctx.fillRect(q[0] - 1, q[1] - 1, 2, 2);
    });
    ctx.globalAlpha = 1;
    const view = state.bounds || full;
    const a = map(view.x0, view.y1), b = map(view.x1, view.y0);
    ctx.strokeStyle = '#2563b8'; ctx.lineWidth = 1.5; ctx.setLineDash([]);
    ctx.strokeRect(a[0], a[1], Math.max(2, b[0] - a[0]), Math.max(2, b[1] - a[1]));
  }
  function draw(state) {
    if (!state.width || !state.height || !state.bounds) return;
    const c = state.ctx;
    c.clearRect(0, 0, state.width, state.height);
    c.fillStyle = '#fff'; c.fillRect(0, 0, state.width, state.height);
    c.strokeStyle = '#eceef1'; c.lineWidth = 1;
    for (let i = 1; i < 5; i++) {
      const x = PAD + i * (state.width - PAD * 2) / 5;
      const y = PAD + i * (state.height - PAD * 2) / 5;
      c.beginPath(); c.moveTo(x, PAD); c.lineTo(x, state.height - PAD); c.stroke();
      c.beginPath(); c.moveTo(PAD, y); c.lineTo(state.width - PAD, y); c.stroke();
    }
    c.strokeStyle = '#33333a'; c.globalAlpha = .72;
    state.shapes.forEach(function (s) {
      if (!finite(s.x0) || !finite(s.y0) || !finite(s.x1) || !finite(s.y1)) return;
      const a = screen(state, Number(s.x0), Number(s.y0)), b = screen(state, Number(s.x1), Number(s.y1));
      c.beginPath(); c.moveTo(a[0], a[1]); c.lineTo(b[0], b[1]); c.stroke();
    });
    c.globalAlpha = state.opacity;
    state.points.forEach(function (p) {
      if (state.hidden.has(p.group)) return;
      const q = screen(state, p.x, p.y);
      if (q[0] < 0 || q[0] > state.width || q[1] < 0 || q[1] > state.height) return;
      c.fillStyle = p.color; c.beginPath(); c.arc(q[0], q[1], state.radius, 0, Math.PI * 2); c.fill();
    });
    if (state.selected.size) {
      c.globalAlpha = 1; c.strokeStyle = '#2563b8'; c.lineWidth = 1.5;
      state.points.forEach(function (p) {
        if (!state.selected.has(p.key) || state.hidden.has(p.group)) return;
        const q = screen(state, p.x, p.y);
        c.beginPath(); c.arc(q[0], q[1], state.radius + 2.5, 0, Math.PI * 2); c.stroke();
      });
      const sb = selectedBounds(state);
      if (sb) {
        const a = screen(state, sb.x0, sb.y1), b = screen(state, sb.x1, sb.y0);
        c.strokeStyle = '#2563b8'; c.lineWidth = 1.25; c.setLineDash([5, 4]);
        c.strokeRect(a[0], a[1], b[0] - a[0], b[1] - a[1]); c.setLineDash([]);
      }
    }
    c.globalAlpha = 1; c.font = '600 12px Inter, sans-serif'; c.textAlign = 'center'; c.textBaseline = 'middle';
    state.centers.forEach(function (p) {
      if (state.hidden.has(p.group)) return;
      const q = screen(state, p.x, p.y), w = c.measureText(p.group).width + 8;
      c.fillStyle = 'rgba(255,255,255,.78)'; c.fillRect(q[0] - w / 2, q[1] - 9, w, 18);
      c.fillStyle = '#252529'; c.fillText(p.group, q[0], q[1]);
    });
    drawMinimap(state);
    if (state.drag && state.drag.lasso && state.drag.path.length > 1) {
      c.strokeStyle='#2563b8';c.lineWidth=1.25;c.setLineDash([5,4]);c.beginPath();c.moveTo(state.drag.path[0][0],state.drag.path[0][1]);state.drag.path.slice(1).forEach(function(p){c.lineTo(p[0],p[1]);});c.stroke();c.setLineDash([]);
    }
  }
  function nearest(state, sx, sy) {
    let best = null, bd = 81;
    state.points.forEach(function (p) {
      if (state.hidden.has(p.group)) return;
      const q = screen(state, p.x, p.y), dx = q[0] - sx, dy = q[1] - sy, d = dx * dx + dy * dy;
      if (d < bd) { bd = d; best = p; }
    });
    return best;
  }
  function inside(point, polygon) {
    let hit=false;
    for(let i=0,j=polygon.length-1;i<polygon.length;j=i++){
      const a=polygon[i],b=polygon[j];
      if(((a[1]>point[1])!==(b[1]>point[1]))&&(point[0]<(b[0]-a[0])*(point[1]-a[1])/(b[1]-a[1])+a[0]))hit=!hit;
    }
    return hit;
  }
  function pushSelection(state) {
    const xs = [], ys = [];
    state.points.forEach(function (p) {
      if (state.selected.has(p.key) && !state.hidden.has(p.group)) { xs.push(p.x); ys.push(p.y); }
    });
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(state.id + '_persistent_selection', xs.length ? {x:xs,y:ys} : null, {priority:'event'});
    }
  }
  function bind(state) {
    const canvas = state.canvas;
    state.toolbar.addEventListener('click', function (e) {
      const button = e.target.closest('button');
      if (!button) return;
      const mode = button.dataset.mode;
      if (mode) {
        state.mode = mode;
        state.toolbar.querySelectorAll('[data-mode]').forEach(function (el) { el.classList.toggle('is-active', el === button); });
        canvas.style.cursor = mode === 'pan' ? 'grab' : 'crosshair';
      } else if (button.dataset.action === 'zoom-in' || button.dataset.action === 'zoom-out') {
        const b = state.bounds;
        const cx = (b.x0 + b.x1) / 2, cy = (b.y0 + b.y1) / 2;
        const factor = button.dataset.action === 'zoom-in' ? .8 : 1.25;
        const hx = (b.x1 - b.x0) * factor / 2, hy = (b.y1 - b.y0) * factor / 2;
        state.bounds = {x0:cx-hx,x1:cx+hx,y0:cy-hy,y1:cy+hy};
        state.zoomed = true; reportZoom(state); schedule(state);
      } else if (button.dataset.action === 'reset') {
        state.bounds = Object.assign({}, state.fullBounds); state.zoomed = false; reportZoom(state); schedule(state);
      } else if (button.dataset.action === 'download') {
        const link = document.createElement('a'); link.download = state.id + '.png'; link.href = canvas.toDataURL('image/png'); link.click();
      }
    });
    canvas.addEventListener('pointerdown', function (e) {
      const r = canvas.getBoundingClientRect();
      const x=e.clientX-r.left,y=e.clientY-r.top;
      state.drag = {x:x,y:y,lastX:x,lastY:y,pan:e.shiftKey||state.mode==='pan',lasso:state.mode==='lasso',path:[[x,y]]};
      canvas.setPointerCapture(e.pointerId);
    });
    canvas.addEventListener('pointermove', function (e) {
      const r = canvas.getBoundingClientRect(), x=e.clientX-r.left, y=e.clientY-r.top;
      if (state.drag) {
        if (state.drag.pan) {
          const a=dataAt(state,state.drag.lastX,state.drag.lastY), b=dataAt(state,x,y), dx=a[0]-b[0],dy=a[1]-b[1];
          state.bounds={x0:state.bounds.x0+dx,x1:state.bounds.x1+dx,y0:state.bounds.y0+dy,y1:state.bounds.y1+dy};
          state.drag.lastX=x; state.drag.lastY=y; state.zoomed=true; reportZoom(state); schedule(state);
        } else if (state.drag.lasso) {
          state.drag.path.push([x,y]);schedule(state);
        } else {
          state.select.style.display='block'; state.select.style.left=Math.min(x,state.drag.x)+'px'; state.select.style.top=Math.min(y,state.drag.y)+'px';
          state.select.style.width=Math.abs(x-state.drag.x)+'px'; state.select.style.height=Math.abs(y-state.drag.y)+'px';
        }
        return;
      }
      const p=nearest(state,x,y);
      if (!p || !p.hover) { state.tip.classList.remove('is-visible'); return; }
      state.tip.textContent=cleanText(p.hover); state.tip.style.left=(x+12)+'px'; state.tip.style.top=(y+12)+'px'; state.tip.classList.add('is-visible');
    });
    canvas.addEventListener('pointerup', function (e) {
      if (!state.drag) return;
      const r=canvas.getBoundingClientRect(), x=e.clientX-r.left,y=e.clientY-r.top, d=state.drag;
      if (d.lasso && d.path.length>2) {
        state.selected.clear();state.points.forEach(function(p){const q=screen(state,p.x,p.y);if(!state.hidden.has(p.group)&&inside(q,d.path))state.selected.add(p.key);});pushSelection(state);schedule(state);
      } else if (!d.pan && Math.abs(x-d.x)>4 && Math.abs(y-d.y)>4) {
        const x0=Math.min(x,d.x),x1=Math.max(x,d.x),y0=Math.min(y,d.y),y1=Math.max(y,d.y);
        state.selected.clear(); state.points.forEach(function(p){const q=screen(state,p.x,p.y);if(!state.hidden.has(p.group)&&q[0]>=x0&&q[0]<=x1&&q[1]>=y0&&q[1]<=y1)state.selected.add(p.key);});
        pushSelection(state); schedule(state);
      }
      state.drag=null; state.select.style.display='none';
    });
    canvas.addEventListener('pointerleave', function(){state.tip.classList.remove('is-visible');});
    canvas.addEventListener('wheel', function(e){
      e.preventDefault(); const r=canvas.getBoundingClientRect(), p=dataAt(state,e.clientX-r.left,e.clientY-r.top), f=e.deltaY>0?1.12:.88,b=state.bounds;
      state.bounds={x0:p[0]+(b.x0-p[0])*f,x1:p[0]+(b.x1-p[0])*f,y0:p[1]+(b.y0-p[1])*f,y1:p[1]+(b.y1-p[1])*f};state.zoomed=true;reportZoom(state);schedule(state);
    },{passive:false});
    canvas.addEventListener('dblclick', function(){state.bounds=Object.assign({},state.fullBounds);state.zoomed=false;reportZoom(state);schedule(state);});
  }
  function reportZoom(state){if(window.Shiny&&Shiny.setInputValue)Shiny.setInputValue(state.id+'_zoom_state',state.zoomed,{priority:'event'});}
  function setLegend(state, meta, colors) {
    let legend=document.getElementById(state.id+'_legend');
    if(!legend){legend=document.createElement('div');legend.id=state.id+'_legend';legend.className='cerebro-projection-legend';state.host.parentElement.insertBefore(legend,state.host);}
    const current=new Set(meta.traces||[]);state.hidden.forEach(function(name){if(!current.has(name))state.hidden.delete(name);});
    legend.innerHTML=''; legend.style.display=meta.legend_position==='none'?'none':'flex';
    (meta.traces||[]).forEach(function(name,i){const item=document.createElement('button'),dot=document.createElement('span');item.type='button';item.className='cerebro-canvas-legend-item';dot.style.backgroundColor=colors[i];item.appendChild(dot);item.appendChild(document.createTextNode(String(name)));item.onclick=function(){state.hidden.has(name)?state.hidden.delete(name):state.hidden.add(name);item.classList.toggle('is-hidden',state.hidden.has(name));if(window.Shiny&&Shiny.setInputValue)Shiny.setInputValue(state.id+'_hidden_groups',Array.from(state.hidden),{priority:'event'});pushSelection(state);schedule(state);};legend.appendChild(item);});
  }
  function setContinuousLegend(state, meta, scale, min, max) {
    let legend=document.getElementById(state.id+'_legend');
    if(!legend){legend=document.createElement('div');legend.id=state.id+'_legend';legend.className='cerebro-projection-legend';state.host.parentElement.insertBefore(legend,state.host);}
    const stops=(Array.isArray(scale)?scale:(palettes[scale]||palettes.Viridis)).map(function(s){return s[1];}).join(','),title=document.createElement('strong'),gradient=document.createElement('span'),lo=document.createElement('small'),hi=document.createElement('small');
    legend.style.display='flex';legend.innerHTML='';title.textContent=String(meta.color_variable||'Value');gradient.className='cerebro-canvas-gradient';gradient.style.background='linear-gradient(90deg,'+stops+')';lo.textContent=Number(min).toFixed(2);hi.textContent=Number(max).toFixed(2);legend.appendChild(title);legend.appendChild(gradient);legend.appendChild(lo);legend.appendChild(hi);
  }
  function common(state, data, extra) {
    state.radius=Math.max(1.5,Number(data.point_size||5)/2);state.opacity=Number(data.point_opacity||.85);state.shapes=(extra&&extra.shapes)||[];
    state.host.dataset.pointCount=String(state.points.length);
    state.fullBounds=paddedBounds(state.points.map(p=>p.x),state.points.map(p=>p.y));
    if(!state.bounds||data.reset_axes)state.bounds=Object.assign({},state.fullBounds);
    resize(state);schedule(state);
  }
  function renderContinuous(meta,data,hover,centers,container,extra){
    const state=ensure(meta.plot_id);if(!state)return false;const vals=data.color||[],range=data.color_range||extent(vals),scale=data.colorscale||'Viridis',reverse=!!data.reversescale,neutral=data.neutral_color;
    state.points=(data.x||[]).map(function(x,i){let t=(Number(vals[i])-Number(range[0]))/(Number(range[1])-Number(range[0])||1);if(reverse)t=1-t;return{x:Number(x),y:Number(data.y[i]),color:neutral||colorAt(scale,t),hover:Array.isArray(hover.text)?hover.text[i]:hover.text,key:key(x,data.y[i]),group:null};}).filter(p=>finite(p.x)&&finite(p.y));state.centers=[];common(state,data,extra);neutral?hideLegend(state):setContinuousLegend(state,meta,scale,range[0],range[1]);return true;
  }
  function renderCategorical(meta,data,hover,centers,container,extra){
    const state=ensure(meta.plot_id);if(!state)return false;state.points=[];(data.x||[]).forEach(function(xs,g){(xs||[]).forEach(function(x,i){const y=data.y[g][i],texts=hover.text&&hover.text[g];state.points.push({x:Number(x),y:Number(y),color:data.color[g],hover:Array.isArray(texts)?texts[i]:texts,key:key(x,y),group:meta.traces[g]});});});state.points=state.points.filter(p=>finite(p.x)&&finite(p.y));state.centers=[];if(centers&&centers.group)centers.group.forEach(function(g,i){state.centers.push({group:g,x:Number(centers.x[i]),y:Number(centers.y[i])});});common(state,data,extra);setLegend(state,meta,data.color||[]);return true;
  }
  function hideLegend(state){const l=document.getElementById(state.id+'_legend');if(l)l.style.display='none';}
  function clearSelection(id){const s=plots.get(id);if(!s)return false;s.selected.clear();pushSelection(s);schedule(s);return true;}
  function toggleZoom(id){const s=plots.get(id);if(!s)return false;if(s.zoomed){s.bounds=Object.assign({},s.fullBounds);s.zoomed=false;}else{const pts=s.points.filter(p=>s.selected.has(p.key));if(!pts.length)return true;s.bounds=paddedBounds(pts.map(p=>p.x),pts.map(p=>p.y));s.zoomed=true;}reportZoom(s);schedule(s);return true;}
  window.cerebroCanvasProjection={renderContinuous:renderContinuous,renderCategorical:renderCategorical,clearSelection:clearSelection,toggleZoom:toggleZoom,deactivate:deactivate};
})();
