/* Revisioned canonical configuration cache for Linked views. */
(function (root, factory) {
  'use strict';
  var api = factory();
  if (typeof module === 'object' && module.exports) module.exports = api;
  else root.cerebroPreparedConfigCache = api;
})(typeof window === 'undefined' ? globalThis : window, function () {
  'use strict';

  function stableSignature(config) {
    var document = Object.assign({}, config || {});
    delete document.created_at;
    return JSON.stringify(document);
  }

  function create(options) {
    options = options || {};
    var capture = options.capture;
    var send = options.send;
    var ready = options.ready || function () { return true; };
    var delay = Number(options.debounceMs) >= 0 ? Number(options.debounceMs) : 250;
    var requestTimeout = Number(options.requestTimeoutMs) > 0
      ? Number(options.requestTimeoutMs) : 5000;
    var setTimer = options.setTimeout || setTimeout;
    var clearTimer = options.clearTimeout || clearTimeout;
    var revision = 0;
    var sequence = 0;
    var timer = null;
    var prepared = null;
    var pending = Object.create(null);
    var waiters = [];

    function nonce() {
      sequence += 1;
      return 'cv-prepare-' + Date.now().toString(36) + '-' + sequence.toString(36);
    }
    function settleWaiters(error, value) {
      var queued = waiters.slice();
      waiters.length = 0;
      queued.forEach(function (waiter) {
        if (error) waiter.reject(error); else waiter.resolve(value);
      });
    }
    function pendingFor(currentRevision, signature) {
      return Object.keys(pending).some(function (key) {
        return pending[key].revision === currentRevision &&
          pending[key].signature === signature;
      });
    }
    function begin(targetRevision) {
      if (targetRevision !== revision || !ready()) return;
      if (timer) clearTimer(timer);
      timer = null;
      var config;
      try { config = capture(); }
      catch (error) { settleWaiters(error); return; }
      var signature = stableSignature(config);
      if (prepared && prepared.signature === signature) {
        prepared.revision = revision;
        settleWaiters(null, prepared);
        return;
      }
      if (pendingFor(revision, signature)) return;
      var requestNonce = nonce();
      pending[requestNonce] = {
        revision: revision,
        signature: signature,
        timer: setTimer(function () {
          var expired = pending[requestNonce];
          if (!expired) return;
          delete pending[requestNonce];
          if (expired.revision === revision && waiters.length) {
            settleWaiters(new Error('Preparing this view timed out. Try again.'));
          }
        }, requestTimeout)
      };
      try {
        send({
          nonce: requestNonce,
          action: 'prepare',
          revision: revision,
          config: config
        });
      } catch (error) {
        clearTimer(pending[requestNonce].timer);
        delete pending[requestNonce];
        settleWaiters(error);
      }
    }
    function schedule(wait) {
      if (timer) clearTimer(timer);
      if (!ready()) { timer = null; return; }
      var targetRevision = revision;
      timer = setTimer(function () { begin(targetRevision); }, wait);
    }
    function invalidate() {
      revision += 1;
      schedule(delay);
      return revision;
    }
    function get() {
      if (!ready()) return Promise.reject(new Error('Select at least one cell first.'));
      if (prepared && prepared.revision === revision) return Promise.resolve(prepared);
      return new Promise(function (resolve, reject) {
        waiters.push({ resolve: resolve, reject: reject });
        schedule(0);
      });
    }
    function receive(result) {
      if (!result || result.action !== 'prepare') return false;
      var record = pending[String(result.nonce || '')];
      if (!record) return false;
      clearTimer(record.timer);
      delete pending[String(result.nonce || '')];
      if (record.revision !== revision) {
        if (waiters.length) schedule(0);
        return false;
      }
      if (!result.ok || typeof result.json !== 'string') {
        settleWaiters(new Error(result.message || 'The view could not be prepared.'));
        return true;
      }
      prepared = {
        revision: revision,
        signature: record.signature,
        json: result.json,
        filename: result.filename || 'linked-views.json',
        selected_cells: Number(result.selected_cells) || 0
      };
      settleWaiters(null, prepared);
      return true;
    }
    function clear() {
      revision += 1;
      if (timer) clearTimer(timer);
      timer = null;
      prepared = null;
      Object.keys(pending).forEach(function (key) {
        clearTimer(pending[key].timer);
      });
      pending = Object.create(null);
      settleWaiters(new Error('The prepared view was cleared.'));
    }

    return {
      invalidate: invalidate,
      get: get,
      receive: receive,
      clear: clear,
      revision: function () { return revision; }
    };
  }

  return { create: create, stableSignature: stableSignature };
});
