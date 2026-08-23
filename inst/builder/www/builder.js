/* Cerebro Dataset Builder: semantic client interaction and accessibility. */
(function () {
  "use strict";

  var tableUploadHighlightTimer = null;
  var narrowManager = window.matchMedia("(max-width: 58rem)");
  var reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
  function syncWorkflowProgressHeight() {
    var progress = document.querySelector(".builder-workflow-progress");
    var topbar = document.querySelector(".topbar");
    var height = progress ? Math.ceil(progress.getBoundingClientRect().height) : 0;
    if (topbar) {
      document.documentElement.style.setProperty(
        "--builder-topbar-height",
        Math.ceil(topbar.getBoundingClientRect().height) + "px"
      );
    }
    document.documentElement.style.setProperty(
      "--builder-workflow-progress-height",
      height + "px"
    );
  }
  window.addEventListener("resize", syncWorkflowProgressHeight);

  var statusTimer = null;
  var lastAnnouncement = "";
  var observedPrimaryAction = null;
  var firstRunKey = "cerebro-builder-first-run-v1";
  var buildDialogHandlerRegistered = false;
  var viewerGroupHandlerRegistered = false;
  var viewerProjectionHandlerRegistered = false;
  var viewerTrajectoryHandlerRegistered = false;
  var spatialSectionHandlerRegistered = false;
  var desiredSpatialSection = null;
  var spatialSectionGeneration = 0;
  var spatialSectionTimers = [];
  var clientUploadSequence = 0;
  var clientImportQueue = [];
  var clientImportFailures = [];
  var activeClientImport = null;
  var uploadConnectionReady = true;
  var importSyncPending = false;
  var serverImportGate = false;
  var clientImportHandlersRegistered = false;
  var viewerDisclosureState = new Map();
  var managerTransitionSequence = 0;
  var datasetMutationsLocked = false;
  var builderConnectionReady = true;
  var builderWorkerReady = false;
  var builderWorkerStatusTimer = null;
  var datasetStartFocusToken = 0;
  var stageFocusToken = 0;
  var datasetSwitchState = {
    target: null,
    authoritative: null,
    generation: 0,
    phase: "idle",
    timeout: null,
    removal: null,
  };
  var coordinateResetSliderIds = new Set([
    "enhance-coordinate_rotation",
    "enhance-point_opacity",
    "enhance-point_size",
  ]);
  var coordinateResetMotionTimers = new Map();
  var dynamicContentEnhancementFrame = null;
  var dynamicContentEnhancementRoots = new Set();
  var spatialScrollbarFrame = null;
  var workflowCompactFrame = null;
  var observedStages = new Set();
  var datasetLoadTimeTimer = null;
  var builderActivityState = {
    phase: "none",
    capabilities: {
      select_dataset: true,
      add_dataset: true,
      edit_dataset: true,
      mutate_datasets: true,
      check_dataset: true,
      create_project: false,
      save_project: false,
      open_project: true,
      prepare_crbs: false,
      navigate_workflow: true,
      build: true,
      page_inert: false,
      warn_before_unload: false,
    },
    busy_title: null,
    busy_message: null,
    busy_detail: null,
    has_project: false,
    open_cancelable: false,
    warn_before_unload: false,
    page_inert: false,
  };

  function scheduleWorkflowCompactState() {
    if (workflowCompactFrame !== null) return;
    workflowCompactFrame = window.requestAnimationFrame(function () {
      workflowCompactFrame = null;
      var workflow = document.getElementById("workflow_progress");
      var topbar = document.querySelector(".topbar");
      if (!workflow || !topbar) return;
      var threshold = topbar.offsetTop + topbar.offsetHeight;
      var compact = workflow.classList.contains("is-workflow-compact");
      var nextCompact = window.scrollY >= (
        compact ? threshold - 48 : threshold
      );
      if (nextCompact === compact) return;
      workflow.classList.toggle("is-workflow-compact", nextCompact);
      syncWorkflowProgressHeight();
    });
  }
  var builderProjectSaveResult = null;
  var builderProjectSaveResultOpen = false;
  var builderProjectCrbDialogActive = false;
  var builderProjectCrbRequestSequence = 0;
  var builderProjectCrbRequestId = null;
  var builderProjectCrbAcknowledgementTimer = null;
  var builderProjectCrbTerminal = false;
  var buildStatusScrollPhase = 0;
  var buildStatusFocusToken = 0;
  var buildOperationActive = false;
  var buildOperationRestoreFocus = null;
  var buildOperationFocusToken = 0;
  var projectOpenOperationActive = false;
  var projectOpenRestoreFocus = null;
  var projectOpenCancelPending = false;
  var normalMotionDuration = 180;
  var authEditor = {
    nextId: 1,
    committed: [],
    snapshot: [],
    open: false,
    saving: false,
  };

  function send(name, value) {
    if (!window.Shiny) return false;
    window.Shiny.setInputValue(name, value, { priority: "event" });
    return true;
  }

  function matchingDynamicElements(roots, selector) {
    var matches = new Set();
    (roots && roots.length ? roots : [document]).forEach(function (root) {
      if (root.nodeType === 1 && root.matches(selector)) matches.add(root);
      root.querySelectorAll(selector).forEach(function (node) {
        matches.add(node);
      });
    });
    return Array.from(matches);
  }

  function matchingDynamicContainers(roots, selector) {
    var matches = new Set(matchingDynamicElements(roots, selector));
    (roots && roots.length ? roots : [document]).forEach(function (root) {
      if (root.nodeType !== 1) return;
      var container = root.closest(selector);
      if (container) matches.add(container);
    });
    return Array.from(matches);
  }

  function datasetSwitchCopy() {
    if (datasetSwitchState.phase === "spatial") {
      return "Preparing Spatial preview…";
    }
    if (datasetSwitchState.phase === "slow") {
      return "This is taking longer than expected…";
    }
    return "Switching dataset…";
  }

  function ensureDatasetSwitchVeil() {
    if (!datasetSwitchState.target) return null;
    var workbench = document.getElementById("workbench");
    var pane = document.getElementById("pane");
    if (!workbench || !pane) return null;
    window.clearTimeout(datasetSwitchState.removal);
    datasetSwitchState.removal = null;
    workbench.setAttribute("aria-busy", "true");
    workbench.inert = true;
    var veil = pane.querySelector(":scope > .builder-dataset-switch-veil");
    if (!veil) {
      veil = document.createElement("div");
      veil.className = "builder-dataset-switch-veil";
      veil.setAttribute("role", "status");
      veil.setAttribute("aria-live", "polite");
      var status = document.createElement("div");
      status.className = "builder-dataset-switch-status";
      var spinner = document.createElement("span");
      spinner.className = "spinner";
      spinner.setAttribute("aria-hidden", "true");
      var text = document.createElement("span");
      text.className = "builder-dataset-switch-copy";
      status.append(spinner, text);
      veil.appendChild(status);
      pane.appendChild(veil);
    }
    veil.classList.remove("is-leaving");
    var text = veil.querySelector(".builder-dataset-switch-copy");
    if (text) text.textContent = datasetSwitchCopy();
    return veil;
  }

  function optimisticallySelectDataset(target) {
    document.querySelectorAll("#ds_ready_list .ds[data-ds]").forEach(
      function (row) {
        var selected = row.dataset.ds === target;
        var control = row.querySelector(".builder-pick");
        row.classList.toggle("is-active", selected);
        if (!control) return;
        if (selected) control.setAttribute("aria-current", "true");
        else control.removeAttribute("aria-current");
      }
    );
  }

  function beginDatasetSwitch(target) {
    if (typeof target !== "string" || !target) return false;
    var selected = document.querySelector(
      "#ds_ready_list .builder-pick[aria-current=true]"
    );
    if (
      !datasetSwitchState.target &&
      selected &&
      selected.dataset.ds === target
    ) return false;

    if (!datasetSwitchState.target) {
      var selectedId = selected ? selected.dataset.ds : null;
      datasetSwitchState.authoritative = selectedId;
    }
    datasetSwitchState.generation += 1;
    datasetSwitchState.target = target;
    datasetSwitchState.phase = "switching";
    window.clearTimeout(datasetSwitchState.timeout);
    var generation = datasetSwitchState.generation;
    optimisticallySelectDataset(target);
    ensureDatasetSwitchVeil();
    datasetSwitchState.timeout = window.setTimeout(function () {
      if (
        datasetSwitchState.target === target &&
        datasetSwitchState.generation === generation
      ) {
        datasetSwitchState.phase = "slow";
        ensureDatasetSwitchVeil();
        datasetSwitchState.timeout = window.setTimeout(function () {
          if (
            datasetSwitchState.target === target &&
            datasetSwitchState.generation === generation
          ) {
            scheduleStatusAnnouncement(
              "The dataset switch did not finish. Try selecting it again."
            );
            settleDatasetSwitch("error");
          }
        }, 22000);
      }
    }, 8000);
    return true;
  }

  function settleDatasetSwitch(outcome) {
    window.clearTimeout(datasetSwitchState.timeout);
    datasetSwitchState.timeout = null;
    if (outcome === "ready") {
      datasetSwitchState.authoritative = datasetSwitchState.target;
    } else if (outcome === "error") {
      optimisticallySelectDataset(datasetSwitchState.authoritative);
    }
    datasetSwitchState.target = null;
    datasetSwitchState.phase = "idle";
    var workbench = document.getElementById("workbench");
    var pane = document.getElementById("pane");
    if (workbench) {
      workbench.removeAttribute("aria-busy");
      workbench.inert = false;
    }
    if (!pane) return;
    var veil = pane.querySelector(":scope > .builder-dataset-switch-veil");
    if (!veil) return;
    window.clearTimeout(datasetSwitchState.removal);
    veil.classList.add("is-leaving");
    datasetSwitchState.removal = window.setTimeout(function () {
      if (veil.isConnected) veil.remove();
      datasetSwitchState.removal = null;
    }, reducedMotion.matches ? 0 : normalMotionDuration);
  }

  function updateDatasetSwitchPhase(message) {
    if (
      !message ||
      message.dataset !== datasetSwitchState.target ||
      Number(message.switch_token) !== datasetSwitchState.generation
    ) return;
    if (message.state === "spatial") {
      datasetSwitchState.phase = "spatial";
      ensureDatasetSwitchVeil();
      return;
    }
    if (message.state === "ready") settleDatasetSwitch("ready");
    if (message.state === "error") settleDatasetSwitch("error");
  }

  function builderOperationElements() {
    var overlay = document.getElementById("builder-operation-overlay");
    return {
      overlay: overlay,
      card: overlay && overlay.querySelector(".builder-operation-overlay-card"),
      title: document.getElementById("builder-operation-overlay-title"),
      message: document.getElementById("builder-operation-overlay-message"),
      detail: document.getElementById("builder-operation-overlay-detail"),
      actions: document.getElementById("builder-operation-overlay-actions"),
    };
  }

  function setBuilderOperationCopy(title, message, detail) {
    var elements = builderOperationElements();
    if (elements.title) elements.title.textContent = title || "";
    if (elements.message) elements.message.textContent = message || "";
    if (elements.detail) elements.detail.textContent = detail || "";
  }

  function setBuilderOperationActions(buttons) {
    var elements = builderOperationElements();
    if (!elements.actions || !elements.card) return;
    elements.actions.replaceChildren();
    buttons.forEach(function (specification) {
      var button = document.createElement("button");
      button.type = "button";
      button.className = specification.primary ? "btn btn-primary" : "btn";
      button.textContent = specification.label;
      button.addEventListener("click", specification.action);
      elements.actions.appendChild(button);
    });
    var hasActions = buttons.length > 0;
    elements.card.classList.toggle("has-actions", hasActions);
    elements.actions.setAttribute("aria-hidden", hasActions ? "false" : "true");
  }

  function cancelBuilderProjectOpen() {
    if (builderActivityState.open_cancelable !== true) return;
    projectOpenCancelPending = true;
    var actions = builderOperationElements().actions;
    var button = actions && actions.querySelector("button");
    if (button) {
      button.disabled = true;
      button.setAttribute("aria-busy", "true");
      button.textContent = "Cancelling…";
    }
    var sent = send("cancel_builder_project_open", { nonce: Date.now() });
    if (!sent) {
      projectOpenCancelPending = false;
      if (button) {
        button.disabled = false;
        button.removeAttribute("aria-busy");
        button.textContent = "Cancel";
      }
    }
  }

  function clearBuilderProjectCrbAcknowledgement() {
    if (builderProjectCrbAcknowledgementTimer !== null) {
      window.clearTimeout(builderProjectCrbAcknowledgementTimer);
    }
    builderProjectCrbAcknowledgementTimer = null;
  }

  function failBuilderProjectCrbRequest(error) {
    if (!builderProjectCrbDialogActive || !builderProjectSaveResultOpen) return;
    clearBuilderProjectCrbAcknowledgement();
    updateBuilderProjectCrbProgress({
      status: "failed",
      completed: 0,
      total: 0,
      error: error || "Builder did not confirm CRB preparation.",
      request_id: builderProjectCrbRequestId,
    });
  }

  function startBuilderProjectCrbRequest(remaining) {
    clearBuilderProjectCrbAcknowledgement();
    builderProjectCrbRequestSequence += 1;
    var requestId = Date.now().toString(36) + "-" +
      builderProjectCrbRequestSequence.toString(36);
    builderProjectCrbRequestId = requestId;
    builderProjectCrbDialogActive = true;
    builderProjectCrbTerminal = false;
    var elements = builderOperationElements();
    if (elements.card) {
      elements.card.classList.remove(
        "is-success",
        "is-error",
        "has-actions"
      );
    }
    setBuilderOperationActions([]);
    if (elements.overlay) elements.overlay.focus({ preventScroll: true });
    setBuilderOperationCopy(
      "Preparing reusable CRBs",
      "Building the checked datasets that do not have a current reusable CRB.",
      "Step 1 of 3 · Planning " + remaining + " CRB" +
        (remaining === 1 ? "" : "s") + " · Keep this page open."
    );
    if (
      !builderConnectionReady || !activityCapability("prepare_crbs")
    ) {
      failBuilderProjectCrbRequest(
        "Reusable CRBs cannot be prepared right now. Reconnect or finish the current Builder operation, then try again."
      );
      return;
    }
    builderProjectCrbAcknowledgementTimer = window.setTimeout(function () {
      if (builderProjectCrbRequestId !== requestId) return;
      failBuilderProjectCrbRequest(
        "Builder did not confirm CRB preparation. Check the connection before trying again."
      );
    }, 15000);
    if (!send("prepare_builder_project_crbs", {
      request_id: requestId,
      nonce: Date.now(),
    })) {
      failBuilderProjectCrbRequest(
        "Builder is not connected, so CRB preparation did not start."
      );
    }
  }

  function closeBuilderProjectSaveResult() {
    clearBuilderProjectCrbAcknowledgement();
    builderProjectCrbDialogActive = false;
    builderProjectCrbRequestId = null;
    builderProjectCrbTerminal = false;
    builderProjectSaveResultOpen = false;
    builderProjectSaveResult = null;
    document.body.classList.remove("builder-project-result-open");
    var elements = builderOperationElements();
    if (elements.overlay) {
      elements.overlay.removeEventListener("keydown", trapDialogKeydown);
      elements.overlay.setAttribute("role", "status");
      elements.overlay.removeAttribute("aria-modal");
    }
    if (elements.card) {
      elements.card.classList.remove("is-result", "is-success", "is-error", "has-actions");
    }
    setBuilderOperationActions([]);
    applyBuilderActivityState();
    updateDialogLock();
    if (elements.overlay) restoreFocus(elements.overlay);
  }

  function showBuilderProjectSaveCompletion(status) {
    if (!builderProjectSaveResultOpen || !builderProjectSaveResult) return;
    var elements = builderOperationElements();
    if (!elements.card) return;
    var checked = Number(builderProjectSaveResult.checked || 0);
    var reusable = Number(builderProjectSaveResult.reusable || 0);
    var remaining = Math.max(0, checked - reusable);
    elements.card.classList.add("is-result");
    elements.card.classList.toggle("is-success", status !== "failed");
    elements.card.classList.toggle("is-error", status === "failed");
    if (status === "failed") {
      setBuilderOperationCopy(
        "Project settings saved",
        "One or more dataset sources could not be copied into the Project.",
        "Your current settings are retained. Use Save project to retry the source files."
      );
    } else {
      setBuilderOperationCopy(
        "Project saved",
        "The project manifest and all dataset sources are safe.",
        remaining > 0
          ? remaining + " checked dataset" + (remaining === 1 ? " has" : "s have") + " no reusable CRB yet."
          : "All checked datasets with prepared CRBs are ready for reuse."
      );
    }
    var buttons = [{ label: "Done", action: closeBuilderProjectSaveResult }];
    if (status === "failed") {
      buttons.push({
        label: "Retry Save project",
        primary: true,
        action: function () {
          closeBuilderProjectSaveResult();
          send("save_builder_project", { nonce: Date.now() });
        },
      });
    } else if (remaining > 0) {
      startBuilderProjectCrbRequest(remaining);
      return;
    }
    setBuilderOperationActions(buttons);
    var firstButton = elements.actions && elements.actions.querySelector("button");
    if (firstButton) firstButton.focus({ preventScroll: true });
  }

  function updateBuilderProjectSourceProgress(message) {
    if (!builderProjectSaveResultOpen || !builderProjectSaveResult) return;
    if (builderProjectCrbDialogActive) return;
    var status = message && message.status;
    if (status === "ready" || status === "failed") {
      showBuilderProjectSaveCompletion(status);
      return;
    }
    if (status !== "syncing") return;
    var completed = Number(message.completed || 0);
    var total = Number(message.total || 0);
    var elements = builderOperationElements();
    if (elements.card) {
      elements.card.classList.add("is-result");
      elements.card.classList.remove("is-success", "is-error", "has-actions");
    }
    setBuilderOperationActions([]);
    setBuilderOperationCopy(
      "Saving dataset sources",
      "Project settings are saved. Dataset files are being copied in the background.",
      total > 0 ? completed + " of " + total + " source files saved · Keep this page open." : "Keep this page open."
    );
  }

  function showBuilderProjectSaveResult(message) {
    if (builderProjectCrbDialogActive) return;
    builderProjectSaveResult = message || {};
    builderProjectSaveResultOpen = true;
    document.body.classList.add("builder-project-result-open");
    var elements = builderOperationElements();
    if (elements.overlay) {
      elements.overlay.setAttribute("aria-hidden", "false");
      elements.overlay.removeEventListener("keydown", trapDialogKeydown);
      prepareDialog(
        elements.overlay,
        document.getElementById("save_builder_project"),
        function () {
          if (elements.card && elements.card.classList.contains("has-actions")) {
            closeBuilderProjectSaveResult();
          }
        },
        function () {
          return document.getElementById("save_builder_project");
        }
      );
    }
    if (elements.card) elements.card.classList.add("is-result");
    var shell = document.querySelector(".builder-shell");
    if (shell) shell.inert = true;
    if (message && message.source_syncing === true) {
      updateBuilderProjectSourceProgress({
        status: "syncing",
        completed: message.completed || 0,
        total: message.total || 0,
      });
    } else {
      showBuilderProjectSaveCompletion("ready");
    }
  }

  function updateBuilderProjectCrbProgress(message) {
    if (!builderProjectSaveResultOpen || !message) return;
    if (!builderProjectCrbDialogActive || builderProjectCrbRequestId === null) {
      return;
    }
    if (message.request_id !== builderProjectCrbRequestId) return;
    var status = message.status || "building";
    if (
      builderProjectCrbTerminal &&
      status !== "ready" &&
      status !== "failed"
    ) return;
    clearBuilderProjectCrbAcknowledgement();
    var completed = Math.max(0, Number(message.completed || 0));
    var total = Math.max(0, Number(message.total || 0));
    var elements = builderOperationElements();
    if (!elements.card) return;
    if (status === "planning") return;
    setBuilderOperationActions([]);
    if (status === "ready") {
      builderProjectCrbTerminal = true;
      elements.card.classList.add("is-result", "is-success");
      elements.card.classList.remove("is-error");
      setBuilderOperationCopy(
        "Project and CRBs saved",
        total > 0
          ? total + " reusable CRB" + (total === 1 ? " is" : "s are") + " ready."
          : "All current reusable CRBs were already up to date.",
        "Stored inside the Project artifacts folder. You can safely close this page or continue working."
      );
      setBuilderOperationActions([
        { label: "Done", action: closeBuilderProjectSaveResult },
      ]);
      return;
    }
    if (status === "failed") {
      builderProjectCrbTerminal = true;
      elements.card.classList.add("is-result", "is-error");
      elements.card.classList.remove("is-success");
      setBuilderOperationCopy(
        "CRB preparation failed",
        message.error || "One or more reusable CRBs could not be prepared.",
        "The Project itself remains saved."
      );
      setBuilderOperationActions([
        { label: "Done", action: closeBuilderProjectSaveResult },
      ]);
      return;
    }
    builderProjectCrbDialogActive = true;
    elements.card.classList.remove("is-success", "is-error");
    setBuilderOperationCopy(
      status === "registering" ? "Saving reusable CRBs" : "Preparing reusable CRBs",
      status === "registering"
        ? "Adding the completed CRBs to the Project."
        : "Building the checked datasets that need a new reusable CRB.",
      status === "registering"
        ? "Step 3 of 3 · " + completed + " of " + total +
          " CRBs prepared · Keep this page open."
        : "Step 2 of 3 · Preparing " + total + " CRB" +
          (total === 1 ? "" : "s") + " · Keep this page open."
    );
  }

  function applyDatasetMutationLock(roots) {
    var activityLocked = !builderConnectionReady || !builderWorkerReady ||
      builderActivityState.capabilities.mutate_datasets === false;
    var selectors = [
      "#dataset_files",
      "#choose_local_datasets",
      "#enhance-choose_local_tables",
      ".builder-file-trigger",
      ".example-btn",
      ".builder-rail-add-browser",
      ".builder-rail-add-local",
      ".builder-reorder",
      ".builder-drop",
      ".builder-retry-import",
      ".builder-remove-import",
      "#undo_remove",
    ].join(", ");
    matchingDynamicElements(roots, selectors).forEach(function (control) {
      var restoreControl = control.matches(
        ".builder-retry-import, .builder-remove-import"
      );
      var retryQueueLocked =
        control.matches(".builder-retry-import") && clientImportQueue.length > 0;
      var controlLocked = datasetMutationsLocked ||
        (activityLocked && !restoreControl) || retryQueueLocked;
      if ("disabled" in control) control.disabled = controlLocked;
      control.setAttribute(
        "aria-disabled",
        controlLocked ? "true" : "false"
      );
      control.classList.toggle(
        "is-dataset-mutation-locked",
        controlLocked
      );
      if (controlLocked) control.setAttribute("tabindex", "-1");
      else if (control.classList.contains("builder-file-trigger")) {
        control.setAttribute("tabindex", "0");
      } else {
        control.removeAttribute("tabindex");
      }
    });
  }

  function activityCapability(name) {
    var importSensitive = [
      "check_dataset",
      "save_project",
      "open_project",
      "prepare_crbs",
      "navigate_workflow",
      "build",
    ].indexOf(name) >= 0;
    var workerSensitive = [
      "add_dataset",
      "edit_dataset",
      "mutate_datasets",
      "check_dataset",
      "open_project",
      "prepare_crbs",
      "navigate_workflow",
      "build",
    ].indexOf(name) >= 0;
    return builderConnectionReady &&
      (!workerSensitive || builderWorkerReady) &&
      (!importSensitive || clientImportQueue.length === 0) &&
      builderActivityState.capabilities[name] !== false;
  }

  function updateBuilderWorkerStatus(message) {
    if (!message || typeof message !== "object") return;
    var state = message.state || "starting";
    var status = document.getElementById("builder-worker-status");
    var title = document.getElementById("builder-worker-status-title");
    var detail = document.getElementById("builder-worker-status-detail");
    builderWorkerReady = state === "ready";
    if (status) {
      if (builderWorkerStatusTimer !== null) {
        window.clearTimeout(builderWorkerStatusTimer);
        builderWorkerStatusTimer = null;
      }
      status.classList.remove("is-dismissed");
      status.classList.toggle("is-starting", state === "starting");
      status.classList.toggle("is-ready", state === "ready");
      status.classList.toggle("is-error", state === "error");
      status.setAttribute("data-worker-state", state);
      if (state === "ready") {
        builderWorkerStatusTimer = window.setTimeout(function () {
          status.classList.add("is-dismissed");
          builderWorkerStatusTimer = null;
        }, 1500);
      }
    }
    if (title && message.title) title.textContent = message.title;
    if (detail) {
      detail.textContent = message.detail || "";
      detail.hidden = state === "ready" || !message.detail;
    }
    applyDatasetMutationLock();
    applyBuilderActivityState();
    if (message.title) scheduleStatusAnnouncement(message.title);
  }

  function setActivityDisabled(control, disabled) {
    if (!control || !("disabled" in control)) return;
    if (disabled) {
      if (!control.hasAttribute("data-builder-activity-lock")) {
        control.setAttribute(
          "data-builder-activity-was-disabled",
          control.disabled ? "true" : "false"
        );
      }
      control.setAttribute("data-builder-activity-lock", "true");
      control.disabled = true;
      if (control.selectize) control.selectize.disable();
      control.setAttribute("aria-disabled", "true");
      return;
    }
    if (!control.hasAttribute("data-builder-activity-lock")) return;
    var wasDisabled = control.getAttribute(
      "data-builder-activity-was-disabled"
    ) === "true";
    control.removeAttribute("data-builder-activity-lock");
    control.removeAttribute("data-builder-activity-was-disabled");
    control.disabled = wasDisabled;
    if (control.selectize && !wasDisabled) control.selectize.enable();
    control.setAttribute("aria-disabled", wasDisabled ? "true" : "false");
  }

  function applyBuilderActivityState(roots) {
    var capabilities = builderActivityState.capabilities || {};
    var projectControls = {
      open_builder_project: activityCapability("open_project"),
      save_builder_project: activityCapability("save_project") ||
        activityCapability("create_project"),
      choose_builder_project_folder: activityCapability("create_project"),
      confirm_builder_project_open: activityCapability("open_project") ||
        (builderActivityState.has_project === true &&
          activityCapability("edit_dataset")),
      choose_saved_project_datasets: activityCapability("edit_dataset"),
      prepare_builder_project_crbs: activityCapability("prepare_crbs"),
      project_resume_current_source: activityCapability("edit_dataset"),
      complete_dataset_check: activityCapability("check_dataset"),
      continue_to_review: activityCapability("navigate_workflow"),
      back_to_settings: activityCapability("navigate_workflow"),
      confirm_review: activityCapability("navigate_workflow"),
      back_to_review: activityCapability("navigate_workflow"),
      choose_output_folder: activityCapability("build"),
      build: activityCapability("build"),
    };
    Object.keys(projectControls).forEach(function (id) {
      setActivityDisabled(
        document.getElementById(id),
        !projectControls[id]
      );
    });
    matchingDynamicElements(roots, ".builder-pick").forEach(function (control) {
      setActivityDisabled(control, !activityCapability("select_dataset"));
    });

    var workspaceLocked = !activityCapability("edit_dataset");
    matchingDynamicElements(
      roots,
      "#builder-workspace input, #builder-workspace select, " +
      "#builder-workspace textarea, #builder-workspace button"
    ).forEach(function (control) {
      setActivityDisabled(control, workspaceLocked);
    });
    matchingDynamicElements(roots, ".builder-workflow-stage-link").forEach(function (link) {
      var locked = !activityCapability("navigate_workflow");
      link.classList.toggle("is-activity-locked", locked);
      link.setAttribute("aria-disabled", locked ? "true" : "false");
      if (locked) link.setAttribute("tabindex", "-1");
      else link.removeAttribute("tabindex");
    });

    var pageInert = builderConnectionReady &&
      builderActivityState.page_inert === true;
    var nextBuildOperationActive = pageInert &&
      builderActivityState.busy_title === "Building output";
    var nextProjectOpenOperationActive = pageInert &&
      builderActivityState.open_cancelable === true &&
      !builderProjectSaveResultOpen;
    var projectOpenPriorFocus = !projectOpenOperationActive &&
      nextProjectOpenOperationActive
      ? document.activeElement
      : null;
    var overlay = document.getElementById("builder-operation-overlay");
    document.body.classList.toggle("builder-page-inert", pageInert);
    if (overlay) {
      overlay.setAttribute(
        "aria-hidden",
        pageInert || builderProjectSaveResultOpen ? "false" : "true"
      );
    }
    if (nextBuildOperationActive && !buildOperationActive) {
      beginBuildOperationFocus(overlay);
    }
    var shell = document.querySelector(".builder-shell");
    if (shell) shell.inert = pageInert || builderProjectSaveResultOpen;
    document.body.classList.toggle(
      "builder-connection-lost",
      !builderConnectionReady
    );
    var title = document.getElementById("builder-operation-overlay-title");
    var message = document.getElementById("builder-operation-overlay-message");
    var detail = document.getElementById("builder-operation-overlay-detail");
    if (
      title &&
      !builderProjectSaveResultOpen &&
      builderActivityState.busy_title &&
      title.textContent !== builderActivityState.busy_title
    ) {
      title.textContent = builderActivityState.busy_title;
    }
    if (
      message &&
      !builderProjectSaveResultOpen &&
      builderActivityState.busy_message &&
      message.textContent !== builderActivityState.busy_message
    ) {
      message.textContent = builderActivityState.busy_message;
    }
    if (detail && !builderProjectSaveResultOpen) {
      var busyDetail = builderActivityState.busy_detail || "";
      if (detail.textContent !== busyDetail) detail.textContent = busyDetail;
    }
    if (!builderProjectSaveResultOpen) {
      if (nextProjectOpenOperationActive && !projectOpenOperationActive) {
        projectOpenCancelPending = false;
      }
      setBuilderOperationActions(
        nextProjectOpenOperationActive
          ? [{ label: "Cancel", action: cancelBuilderProjectOpen }]
          : []
      );
      if (nextProjectOpenOperationActive && projectOpenCancelPending) {
        var pendingCancel = builderOperationElements().actions;
        pendingCancel = pendingCancel && pendingCancel.querySelector("button");
        if (pendingCancel) {
          pendingCancel.disabled = true;
          pendingCancel.setAttribute("aria-busy", "true");
          pendingCancel.textContent = "Cancelling…";
        }
      }
    }
    if (nextProjectOpenOperationActive && !projectOpenOperationActive) {
      projectOpenRestoreFocus = canRestoreFocus(projectOpenPriorFocus)
        ? projectOpenPriorFocus
        : document.getElementById("open_builder_project");
      if (overlay) {
        overlay.setAttribute("role", "dialog");
        overlay.setAttribute("aria-modal", "true");
      }
      var cancelOpen = builderOperationElements().actions;
      cancelOpen = cancelOpen && cancelOpen.querySelector("button");
      if (cancelOpen) cancelOpen.focus({ preventScroll: true });
    } else if (
      projectOpenOperationActive &&
      !nextProjectOpenOperationActive &&
      !builderProjectSaveResultOpen
    ) {
      if (overlay) {
        overlay.setAttribute("role", "status");
        overlay.removeAttribute("aria-modal");
      }
      var openRestoreTarget = projectOpenRestoreFocus;
      projectOpenRestoreFocus = null;
      projectOpenCancelPending = false;
      window.setTimeout(function () {
        if (document.querySelector('.modal.show, [aria-modal="true"]')) return;
        if (canRestoreFocus(openRestoreTarget)) {
          openRestoreTarget.focus({ preventScroll: true });
        }
      }, 100);
    }
    projectOpenOperationActive = nextProjectOpenOperationActive;
    var priorBuildOperationActive = buildOperationActive;
    buildOperationActive = nextBuildOperationActive;
    if (priorBuildOperationActive && !buildOperationActive) {
      restoreBuildOperationFocus();
    }
    applyDatasetMutationLock(roots);
  }

  function beginBuildOperationFocus(overlay) {
    buildOperationFocusToken += 1;
    var active = document.activeElement;
    if (
      !buildOperationRestoreFocus &&
      canRestoreFocus(active) &&
      active !== overlay
    ) {
      buildOperationRestoreFocus = active;
    } else if (!buildOperationRestoreFocus) {
      buildOperationRestoreFocus = document.getElementById("build");
    }
    if (overlay) overlay.focus({ preventScroll: true });
  }

  function restoreBuildOperationFocus() {
    buildOperationFocusToken += 1;
    var token = buildOperationFocusToken;
    var attempts = 0;
    function apply() {
      if (token !== buildOperationFocusToken) return;
      var heading = document.querySelector(".result-card h2");
      var action = document.querySelector(
        ".result-card .builder-result-actions button, " +
          ".result-card .builder-recovery-action button"
      );
      var target = heading || action;
      if (heading) heading.setAttribute("tabindex", "-1");
      if (!canRestoreFocus(target)) target = buildOperationRestoreFocus;
      if (!canRestoreFocus(target)) target = document.getElementById("build");
      if (canRestoreFocus(target)) {
        target.focus({ preventScroll: true });
        buildOperationRestoreFocus = null;
        return;
      }
      attempts += 1;
      if (attempts >= 12) {
        buildOperationRestoreFocus = null;
        return;
      }
      window.setTimeout(apply, 50);
    }
    window.setTimeout(apply, 0);
  }

  function focusBuildStatus() {
    var host = document.getElementById("build-stage-status");
    if (!host || !host.isConnected) {
      return false;
    }
    var topbar = document.querySelector(".topbar");
    var topbarBottom = topbar ? topbar.getBoundingClientRect().bottom : 0;
    host.style.scrollMarginTop = Math.max(0, topbarBottom + 12) + "px";
    host.setAttribute("tabindex", "-1");
    host.scrollIntoView({
      block: "start",
      behavior: reducedMotion.matches ? "auto" : "smooth",
    });
    host.focus({ preventScroll: true });
    return true;
  }

  function scheduleBuildStatusFocus() {
    buildStatusFocusToken += 1;
    var token = buildStatusFocusToken;
    var attempts = 0;
    function apply() {
      if (token !== buildStatusFocusToken) return;
      if (focusBuildStatus()) return;
      attempts += 1;
      if (attempts < 12) window.setTimeout(apply, 50);
    }
    apply();
  }

  function showImmediateBuildStatus() {
    var host = document.getElementById("build-stage-status");
    var output = document.getElementById("build_stage_status_content");
    if (!host || !output) return;
    buildOperationRestoreFocus = document.activeElement ||
      document.getElementById("build");

    var previous = host.querySelector(
      ":scope > .builder-build-status-section.is-client-build-status"
    );
    if (previous) previous.remove();

    var section = document.createElement("section");
    section.className = [
      "builder-stage-section",
      "builder-build-status-section",
      "is-client-build-status",
    ].join(" ");
    var heading = document.createElement("h3");
    heading.textContent = "Build status";
    var waiting = document.createElement("div");
    waiting.className = "builder-build-waiting";
    var spinner = document.createElement("span");
    spinner.className = "spinner";
    spinner.setAttribute("aria-hidden", "true");
    var label = document.createElement("span");
    label.textContent = "Preparing build…";
    waiting.append(spinner, label);
    section.append(heading, waiting);
    host.insertBefore(section, output);

    buildStatusScrollPhase = 1;
    scheduleBuildStatusFocus();
    buildStatusScrollPhase = 2;
    scheduleStatusAnnouncement("Preparing build.");
  }

  function authCopy(accounts) {
    return accounts.map(function (account) {
      return { id: account.id, username: account.username, password: account.password };
    });
  }

  function authAccountRow(account) {
    var row = document.createElement("div");
    row.className = "builder-auth-row";
    row.dataset.authId = account.id;
    [["Username", "text", "builder-auth-username", "username", account.username],
      ["Password", "password", "builder-auth-password", "new-password", account.password]
    ].forEach(function (spec) {
      var label = document.createElement("label");
      label.textContent = spec[0];
      var input = document.createElement("input");
      input.type = spec[1];
      input.className = spec[2];
      input.autocomplete = spec[3];
      input.value = spec[4];
      label.appendChild(input);
      row.appendChild(label);
    });
    var remove = document.createElement("button");
    remove.type = "button";
    remove.className = "btn builder-auth-remove";
    remove.textContent = "Remove";
    row.appendChild(remove);
    return row;
  }

  function authRows() {
    return Array.from(document.querySelectorAll(".builder-auth-row")).map(function (row) {
      return {
        id: row.dataset.authId,
        username: row.querySelector(".builder-auth-username").value,
        password: row.querySelector(".builder-auth-password").value,
      };
    });
  }

  function authRender(accounts) {
    var root = document.querySelector("[data-auth-rows]");
    if (!root) return;
    root.replaceChildren();
    accounts.forEach(function (account) { root.appendChild(authAccountRow(account)); });
  }

  function authNewAccount() {
    return { id: "auth-account-" + authEditor.nextId++, username: "", password: "" };
  }

  function clearAuthLiveInputs() {
    document.querySelectorAll(".builder-auth-row input").forEach(function (input) {
      input.value = "";
    });
  }

  function clearAuthSecrets() {
    clearAuthLiveInputs();
    authEditor.committed = [];
    authEditor.snapshot = [];
  }

  function clearAuthError() {
    var error = document.getElementById("builder-auth-error");
    if (!error) return;
    error.textContent = "";
    error.hidden = true;
  }

  function restoreAuthSnapshot() {
    authEditor.committed = authCopy(authEditor.snapshot);
    authRender(authEditor.snapshot);
  }

  function authOpenFocusFallback() {
    var trigger = document.querySelector(".builder-auth-open");
    var options = trigger && trigger.closest("details");
    if (options) options.open = true;
    return trigger;
  }

  function setAuthSaving(pendingNonce) {
    var saving = typeof pendingNonce === "number" && Number.isFinite(pendingNonce);
    authEditor.saving = saving ? pendingNonce : false;
    var dialog = document.getElementById("builder-auth-dialog");
    if (!dialog) return;
    dialog.querySelectorAll("input, button").forEach(function (control) {
      control.disabled = saving;
    });
  }

  function closeAuthDialog(restore) {
    var backdrop = document.getElementById("builder-auth-backdrop");
    var dialog = document.getElementById("builder-auth-dialog");
    if (!backdrop || !dialog) return;
    authEditor.open = false;
    backdrop.classList.remove("is-visible");
    dialog.classList.remove("is-visible");
    backdrop.hidden = true;
    document.body.classList.remove("builder-dialog-open");
    if (restore) restoreFocus(dialog);
  }

  function openAuthDialog(trigger) {
    var backdrop = document.getElementById("builder-auth-backdrop");
    var dialog = document.getElementById("builder-auth-dialog");
    if (!backdrop || !dialog) return;
    clearAuthError();
    setAuthSaving(false);
    authEditor.snapshot = authCopy(authEditor.committed);
    if (!authEditor.committed.length) authRender([authNewAccount()]);
    else authRender(authEditor.committed);
    authEditor.open = true;
    backdrop.hidden = false;
    prepareDialog(
      dialog,
      trigger,
      function () {
        if (authEditor.saving) return;
        restoreAuthSnapshot();
        closeAuthDialog(true);
      },
      authOpenFocusFallback
    );
    showTransientLayer(backdrop, dialog);
  }

  function focusableElements(root) {
    return Array.from(
      root.querySelectorAll(
        "button:not([disabled]), [href], input:not([disabled]), " +
          "select:not([disabled]), textarea:not([disabled]), " +
          '[tabindex]:not([tabindex="-1"])'
      )
    ).filter(function (element) {
      return element.getClientRects().length > 0;
    });
  }

  function canRestoreFocus(target) {
    return Boolean(
      target &&
      document.contains(target) &&
      !target.disabled &&
      target.getClientRects().length > 0
    );
  }

  function restoreFocus(dialog) {
    var target = dialog && dialog.__builderRestoreFocus;
    var fallback = dialog && dialog.__builderRestoreFocusFallback;
    var restoreToken = dialog ?
      (dialog.__builderFocusRestoreToken || 0) + 1 : 0;
    if (dialog) dialog.__builderFocusRestoreToken = restoreToken;
    var lastTarget = null;
    var attempts = 0;
    function attempt() {
      if (dialog && dialog.__builderFocusRestoreToken !== restoreToken) return;
      var active = document.activeElement;
      var focusWasLost = !active || active === document.body ||
        active === document.documentElement ||
        (dialog && dialog.contains(active)) || active === lastTarget;
      // Do not steal focus if the user has already moved it elsewhere while a
      // Shiny redraw is settling. Re-check the intended target briefly because
      // a redraw can replace the button after the first successful focus,
      // leaving document.body active again.
      if (!focusWasLost) return;
      var nextTarget = canRestoreFocus(target) ? target : null;
      if (!nextTarget && typeof fallback === "function") {
        nextTarget = fallback();
      }
      if (canRestoreFocus(nextTarget)) {
        nextTarget.focus();
        lastTarget = nextTarget;
        target = nextTarget;
      }
      attempts++;
      if (attempts < 12) {
        window.setTimeout(attempt, attempts === 1 ? 50 : 250);
      }
    }
    window.setTimeout(attempt, 0);
  }

  function removeDatasetFocusFallback() {
    var rail = document.querySelector(".rail");
    if (
      narrowManager.matches &&
      rail &&
      rail.classList.contains("is-manager-open")
    ) {
      var close = rail.querySelector(".rail-manager-close");
      if (canRestoreFocus(close)) return close;
      var managerItems = focusableElements(rail);
      if (managerItems.length) return managerItems[0];
      if (canRestoreFocus(rail)) return rail;
    }
    var fileTrigger = document.querySelector(".builder-file-trigger");
    if (canRestoreFocus(fileTrigger)) return fileTrigger;
    var railItems = rail ? focusableElements(rail) : [];
    return railItems.length ? railItems[0] : null;
  }

  function trapDialogKeydown(event) {
    var dialog = event.currentTarget;
    if (event.key === "Escape") {
      event.preventDefault();
      event.stopPropagation();
      if (dialog.__builderClose) dialog.__builderClose();
      return;
    }
    if (event.key !== "Tab") return;
    var items = focusableElements(dialog);
    if (!items.length) {
      event.preventDefault();
      dialog.focus();
      return;
    }
    var first = items[0];
    var last = items[items.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    }
  }

  function prepareDialog(dialog, trigger, close, restoreFallback) {
    dialog.__builderFocusRestoreToken =
      (dialog.__builderFocusRestoreToken || 0) + 1;
    dialog.setAttribute("role", "dialog");
    dialog.setAttribute("aria-modal", "true");
    dialog.setAttribute("tabindex", "-1");
    dialog.__builderRestoreFocus = trigger || document.activeElement;
    dialog.__builderRestoreFocusFallback = restoreFallback;
    dialog.__builderClose = close;
    dialog.addEventListener("keydown", trapDialogKeydown);
    document.body.classList.add("builder-dialog-open");
    window.setTimeout(function () {
      var items = focusableElements(dialog);
      (items[0] || dialog).focus();
    }, 0);
  }

  function showTransientLayer(backdrop, dialog, visibleClass) {
    var previous = backdrop && backdrop.__builderTransientState;
    if (previous && previous.cancel) previous.cancel();
    var state = { closing: false };
    if (backdrop) backdrop.__builderTransientState = state;
    window.requestAnimationFrame(function () {
      if (
        state.closing ||
        !backdrop ||
        backdrop.__builderTransientState !== state ||
        !backdrop.isConnected ||
        !dialog ||
        !dialog.isConnected
      ) return;
      backdrop.classList.add("is-visible");
      dialog.classList.add(visibleClass || "is-visible");
    });
  }

  function removeTransientLayer(
    backdrop,
    dialog,
    visibleClass,
    complete,
    removeBackdrop
  ) {
    var state = backdrop && backdrop.__builderTransientState;
    if (!state) {
      state = { closing: false };
      if (backdrop) backdrop.__builderTransientState = state;
    }
    state.closing = true;
    if (backdrop) backdrop.classList.remove("is-visible");
    if (dialog) dialog.classList.remove(visibleClass || "is-visible");

    var finished = false;
    var timeout = null;
    function cleanup() {
      if (timeout !== null) window.clearTimeout(timeout);
      if (dialog) dialog.removeEventListener("transitionend", onTransitionEnd);
    }
    function finish() {
      if (finished) return;
      finished = true;
      cleanup();
      if (
        backdrop &&
        backdrop.__builderTransientState === state
      ) {
        delete backdrop.__builderTransientState;
      }
      if (removeBackdrop !== false && backdrop) backdrop.remove();
      if (complete) complete();
    }
    function onTransitionEnd(event) {
      if (event.target !== dialog) return;
      finish();
    }
    state.cancel = function () {
      if (finished) return;
      finished = true;
      cleanup();
    };

    if (reducedMotion.matches || window.__builderMotionDuration === 0) {
      finish();
      return;
    }
    if (dialog) dialog.addEventListener("transitionend", onTransitionEnd);
    timeout = window.setTimeout(
      finish,
      window.__builderMotionDuration + 60
    );
  }

  function pageShortcutBlocked() {
    if (
      document.body.classList.contains("modal-open") ||
      document.body.classList.contains("builder-dialog-open")
    ) return true;
    return Array.from(
      document.querySelectorAll('[aria-modal="true"]')
    ).some(function (dialog) {
      return !dialog.closest("[hidden]") && dialog.getClientRects().length > 0;
    });
  }

  function isTextInput(target) {
    return Boolean(target && target.closest(
      "input, textarea, select, [contenteditable='true'], .selectize-input"
    ));
  }

  function setupFirstRun() {
    var guide = document.querySelector(".builder-first-run[data-first-run]");
    if (!guide) return;
    if (document.querySelector("#configure_actions .btn-dataset-check, #continue_to_review")) {
      guide.hidden = true;
      return;
    }
    if (guide.dataset.ready === "true") return;
    guide.dataset.ready = "true";
    var dismissed = false;
    try { dismissed = window.localStorage.getItem(firstRunKey) === "dismissed"; } catch (error) {}
    if (dismissed) guide.hidden = true;
  }

  function openDatasetPicker() {
    if (datasetMutationsLocked || !activityCapability("add_dataset")) return;
    var picker = document.createElement("input");
    picker.type = "file";
    picker.multiple = true;
    var transport = document.getElementById("dataset_files");
    picker.accept = transport ? transport.accept : ".rds,.qs,.qs2";
    picker.addEventListener("change", function () {
      enqueueClientFiles(picker.files);
    }, { once: true });
    picker.click();
  }

  function setupDatasetDropzones(roots) {
    matchingDynamicElements(roots, ".builder-dataset-dropzone").forEach(function (dropzone) {
      if (dropzone.dataset.dropzoneBound === "true") return;
      dropzone.dataset.dropzoneBound = "true";
      dropzone.addEventListener("click", openDatasetPicker);
      dropzone.addEventListener("keydown", function (event) {
        if (event.key !== "Enter" && event.key !== " ") return;
        event.preventDefault();
        openDatasetPicker();
      });
      dropzone.addEventListener("dragover", function (event) {
        event.preventDefault();
        if (!activityCapability("add_dataset")) return;
        dropzone.classList.add("is-drag-over");
      });
      dropzone.addEventListener("dragleave", function (event) {
        if (event.relatedTarget && dropzone.contains(event.relatedTarget)) return;
        dropzone.classList.remove("is-drag-over");
      });
      dropzone.addEventListener("drop", function (event) {
        event.preventDefault();
        dropzone.classList.remove("is-drag-over");
        if (!activityCapability("add_dataset")) return;
        enqueueClientFiles(event.dataTransfer && event.dataTransfer.files);
      });
    });
  }

  function clientQueueStatus(entry, index) {
    if (entry.state === "paused") return "Connection lost · Waiting to restore the import state…";
    if (entry.state === "unknown") return "Import state could not be restored";
    if (entry.state === "awaiting_dispatch") return "Preparing upload…";
    if (entry.state === "awaiting_upload") return "Waiting to upload…";
    if (entry.state === "dispatching" || entry.state === "uploading") return "Uploading…";
    if (entry.state === "awaiting_accept") return "Waiting for the server…";
    if (entry.state === "error" || entry.state === "rejected") {
      return entry.error || "Could not start this import";
    }
    return "Waiting · " + String(index + 1) + " in queue" +
      (entry.duplicateHint ? " · Possible duplicate" : "");
  }

  function applyClientImportQueueLock(roots) {
    var locked = clientImportQueue.length > 0;
    matchingDynamicElements(roots, ".builder-retry-import").forEach(function (control) {
      control.disabled = locked;
      control.setAttribute("aria-disabled", locked ? "true" : "false");
    });
  }

  function reportClientImportQueueState() {
    send("builder_client_import_state", {
      nonce: Date.now(),
      pending: clientImportQueue.length,
    });
  }

  function renderClientImportQueue() {
    reportClientImportQueueState();
    applyBuilderActivityState();
    var container = document.getElementById("ds_client_import_queue");
    if (!container) return;
    container.replaceChildren();
    var sequence = document.querySelectorAll(
      "#ds_ready_list > .ds, #ds_import_list .ds"
    ).length;
    clientImportQueue.concat(clientImportFailures).forEach(function (entry, index) {
      if (entry.serverId && entry === activeClientImport) return;
      sequence += 1;
      var row = document.createElement("div");
      row.className = "ds ds--import ds--client-upload";
      row.dataset.clientImportId = entry.clientId;
      row.dataset.loadState = entry.state;
      var ordinal = document.createElement("span");
      ordinal.className = "ds-idx";
      ordinal.textContent = String(sequence);
      var body = document.createElement("span");
      body.className = "ds-body";
      var name = document.createElement("span");
      name.className = "nm";
      name.textContent = entry.name;
      var status = document.createElement("span");
      status.className = "builder-import-status";
      status.textContent = clientQueueStatus(entry, index);
      body.appendChild(name);
      body.appendChild(status);
      row.appendChild(ordinal);
      row.appendChild(body);
      var dot = document.createElement("span");
      dot.className = "ds-state-dot";
      dot.setAttribute("aria-hidden", "true");
      row.appendChild(dot);
      if (
        entry !== activeClientImport &&
        !entry.serverId &&
        !["error", "rejected"].includes(entry.state)
      ) {
        var cancel = document.createElement("button");
        cancel.type = "button";
        cancel.className = "btn btn-remove-soft builder-cancel-client-import";
        cancel.dataset.clientImportId = entry.clientId;
        cancel.setAttribute("aria-label", "Cancel queued import " + entry.name);
        cancel.textContent = "Cancel";
        row.appendChild(cancel);
      } else if (clientImportFailures.includes(entry)) {
        if (entry.outcome === "error") {
          var retry = document.createElement("button");
          retry.type = "button";
          retry.className = "btn builder-retry-client-import";
          retry.dataset.clientImportId = entry.clientId;
          retry.textContent = "Retry";
          row.appendChild(retry);
        }
        var remove = document.createElement("button");
        remove.type = "button";
        remove.className = "btn btn-remove-soft builder-remove-client-failure";
        remove.dataset.clientImportId = entry.clientId;
        remove.textContent = "Remove";
        row.appendChild(remove);
      }
      container.appendChild(row);
    });
    applyClientImportQueueLock();
  }

  function failClientDispatch(entry, message) {
    entry.state = "error";
    entry.outcome = "error";
    entry.error = message;
    scheduleStatusAnnouncement(entry.name + ". " + message);
    clientImportQueue.shift();
    activeClientImport = null;
    clientImportFailures.push(entry);
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function startFileTransport(entry) {
    var transport = document.getElementById("dataset_files");
    if (!transport) {
      failClientDispatch(entry, "The upload transport is unavailable.");
      return;
    }
    entry.state = "uploading";
    try {
      var transfer = new DataTransfer();
      transfer.items.add(entry.file);
      transport.files = transfer.files;
    } catch (error) {
      failClientDispatch(entry, "This browser could not prepare the upload.");
      return;
    }
    if (
      !transport.files ||
      transport.files.length !== 1 ||
      transport.files[0].name !== entry.file.name ||
      transport.files[0].size !== entry.file.size
    ) {
      failClientDispatch(entry, "This browser could not prepare the selected file.");
      return;
    }
    renderClientImportQueue();
    transport.dispatchEvent(new Event("change", { bubbles: true }));
    entry.state = "awaiting_accept";
    renderClientImportQueue();
  }

  function dispatchFileImport(entry) {
    entry.state = "awaiting_dispatch";
    renderClientImportQueue();
    send("builder_client_import_dispatch", {
      client_id: entry.clientId,
      name: entry.name,
      size: entry.size,
      nonce: Date.now(),
    });
  }

  function handleClientImportDispatchReady(message) {
    if (!activeClientImport || !message || message.client_id !== activeClientImport.clientId) return;
    if (activeClientImport.kind !== "file" || activeClientImport.serverId) return;
    if (!["awaiting_dispatch", "awaiting_upload"].includes(activeClientImport.state)) return;
    startFileTransport(activeClientImport);
  }

  function dispatchExampleImport(entry) {
    entry.state = "awaiting_accept";
    renderClientImportQueue();
    send("builder_import_example", {
      example: entry.exampleId,
      client_id: entry.clientId,
      nonce: Date.now(),
    });
  }

  function dispatchNextClientImport() {
    if (!uploadConnectionReady) return;
    if (importSyncPending) return;
    if (serverImportGate) return;
    if (activeClientImport) return;
    if (!clientImportQueue.length) return;
    activeClientImport = clientImportQueue[0];
    if (activeClientImport.kind === "file") dispatchFileImport(activeClientImport);
    else dispatchExampleImport(activeClientImport);
  }

  function enqueueClientFiles(fileList) {
    if (!activityCapability("add_dataset")) return;
    var files = Array.from(fileList || []);
    files.forEach(function (file) {
      var duplicateHint = clientImportQueue.some(function (queued) {
        return queued.kind === "file" &&
          queued.name === file.name &&
          queued.size === file.size &&
          queued.lastModified === file.lastModified;
      });
      clientUploadSequence += 1;
      clientImportQueue.push({
        clientId: "client-import-" + clientUploadSequence,
        kind: "file",
        file: file,
        name: file.name,
        size: file.size,
        lastModified: file.lastModified,
        exampleId: null,
        serverId: null,
        state: "queued",
        outcome: null,
        error: null,
        duplicateHint: duplicateHint,
      });
    });
    if (!files.length) return;
    renderClientImportQueue();
    if (narrowManager.matches) closeDatasetManager();
    dispatchNextClientImport();
  }

  function enqueueExample(example) {
    if (!activityCapability("add_dataset")) return;
    clientUploadSequence += 1;
    clientImportQueue.push({
      clientId: "client-import-" + clientUploadSequence,
      kind: "example",
      file: null,
      name: example.name,
      size: null,
      lastModified: null,
      exampleId: example.exampleId,
      serverId: null,
      state: "queued",
      outcome: null,
      error: null,
    });
    renderClientImportQueue();
    if (narrowManager.matches) closeDatasetManager();
    dispatchNextClientImport();
  }

  function cancelClientImport(clientId) {
    var index = clientImportQueue.findIndex(function (entry) {
      return entry.clientId === clientId;
    });
    if (index < 0) return;
    var entry = clientImportQueue[index];
    if (entry === activeClientImport) {
      if (!entry.serverId) return;
      send("cancel_pending_upload", { id: entry.serverId, nonce: Date.now() });
      return;
    }
    clientImportQueue.splice(index, 1);
    renderClientImportQueue();
  }

  function removeClientImportFailure(clientId) {
    clientImportFailures = clientImportFailures.filter(function (entry) {
      return entry.clientId !== clientId;
    });
    renderClientImportQueue();
  }

  function retryClientImportFailure(clientId) {
    var index = clientImportFailures.findIndex(function (entry) {
      return entry.clientId === clientId;
    });
    if (index < 0) return;
    var entry = clientImportFailures.splice(index, 1)[0];
    clientUploadSequence += 1;
    entry.clientId = "client-import-" + clientUploadSequence;
    entry.state = "queued";
    entry.outcome = null;
    entry.error = null;
    entry.serverId = null;
    clientImportQueue.push(entry);
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function handleClientImportAccepted(message) {
    if (!activeClientImport || !message || message.client_id !== activeClientImport.clientId) return;
    activeClientImport.serverId = message.server_id || null;
    activeClientImport.state = "reading";
    renderClientImportQueue();
  }

  function handleClientImportRelease(message) {
    if (!activeClientImport || !message) return;
    if (message.client_id !== activeClientImport.clientId) return;
    if (activeClientImport.serverId && message.server_id && message.server_id !== activeClientImport.serverId) return;
    var releasedEntry = activeClientImport;
    activeClientImport.outcome = message.outcome;
    activeClientImport.error = message.message || null;
    if (["error", "rejected"].includes(message.outcome)) {
      scheduleStatusAnnouncement(
        activeClientImport.name + ". " +
          (activeClientImport.error || "The import could not be completed.")
      );
    }
    clientImportQueue.shift();
    activeClientImport = null;
    if (["error", "rejected"].includes(message.outcome) && !message.server_id) {
      clientImportFailures.push(releasedEntry);
    }
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function failClientImportReconciliation(entry, message) {
    entry.state = "error";
    entry.outcome = "error";
    entry.error = message;
    entry.serverId = null;
    var index = clientImportQueue.indexOf(entry);
    if (index >= 0) clientImportQueue.splice(index, 1);
    if (activeClientImport === entry) activeClientImport = null;
    importSyncPending = false;
    if (!clientImportFailures.includes(entry)) clientImportFailures.push(entry);
    scheduleStatusAnnouncement(entry.name + ". " + message);
    renderClientImportQueue();
    dispatchNextClientImport();
  }

  function handleClientImportSync(message) {
    var records = Array.isArray(message && message.imports) ? message.imports : [];
    serverImportGate = Boolean(message && message.server_busy);
    if (!activeClientImport) {
      importSyncPending = false;
      dispatchNextClientImport();
      return;
    }
    var entry = activeClientImport;
    var record = records.find(function (item) {
      return item && item.client_id === entry.clientId;
    });
    if (!record) {
      if (!entry.serverId && ["uploading", "awaiting_accept", "paused"].includes(entry.state)) {
        entry.serverId = null;
        entry.state = "queued";
        activeClientImport = null;
        importSyncPending = false;
        renderClientImportQueue();
        dispatchNextClientImport();
        return;
      }
      failClientImportReconciliation(entry, "The server could not match this import after reconnecting.");
      return;
    }
    if (entry.serverId && record.server_id !== entry.serverId) {
      failClientImportReconciliation(entry, "The restored import identity did not match.");
      return;
    }
    entry.serverId = record.server_id || entry.serverId;
    if (["ready", "error", "cancelled", "rejected"].includes(record.state)) {
      importSyncPending = false;
      handleClientImportRelease({
        client_id: entry.clientId,
        server_id: entry.serverId,
        outcome: record.state,
        message: record.message || null,
      });
      return;
    }
    if (record.state === "awaiting_upload") {
      var uploadWasStarted = ["uploading", "awaiting_accept"].includes(entry.stateBeforePause);
      entry.state = uploadWasStarted ? "awaiting_accept" : "awaiting_upload";
      importSyncPending = false;
      renderClientImportQueue();
      if (!uploadWasStarted) startFileTransport(entry);
      return;
    }
    entry.state = record.state || entry.stateBeforePause || "unknown";
    importSyncPending = false;
    renderClientImportQueue();
  }

  function updateDialogLock() {
    var hasVisibleModal = Array.from(
      document.querySelectorAll('[aria-modal="true"]')
    ).some(function (dialog) {
      return !dialog.closest("[hidden]");
    });
    document.body.classList.toggle(
      "builder-dialog-open",
      hasVisibleModal
    );
  }

  function setRailDesktopSemantics(rail) {
    rail.setAttribute("role", "complementary");
    rail.setAttribute("aria-label", "Datasets");
    rail.removeAttribute("aria-modal");
    rail.removeAttribute("aria-hidden");
  }

  function updateRailSummary() {
    var summary = document.querySelector(".rail-summary");
    var rail = document.querySelector(".rail");
    if (!summary || !rail) return;
    var current = rail.querySelector(".ds.is-active");
    var name = current && current.querySelector(".nm");
    var readiness = current && current.querySelector(".rail-readiness-status");
    var nextName = name ? name.textContent.trim() : "No dataset selected";
    var nextState = [
      readiness ? readiness.textContent.trim() : "",
    ]
      .filter(Boolean)
      .join(" · ");
    var nameOutput = summary.querySelector(".rail-summary-name");
    var stateOutput = summary.querySelector(".rail-summary-state");
    if (nameOutput.textContent !== nextName) nameOutput.textContent = nextName;
    if (stateOutput.textContent !== nextState) stateOutput.textContent = nextState;
    if (nextState) scheduleStatusAnnouncement(nextName + ". " + nextState + ".");
  }

  function closeDatasetManager() {
    var rail = document.querySelector(".rail");
    var summary = document.querySelector(".rail-summary");
    var backdrop = document.querySelector(".rail-manager-backdrop");
    if (!rail || !rail.classList.contains("is-manager-open")) return;
    managerTransitionSequence += 1;
    var closeSequence = managerTransitionSequence;
    if (summary) summary.setAttribute("aria-expanded", "false");
    rail.removeEventListener("keydown", trapDialogKeydown);
    setRailDesktopSemantics(rail);
    if (narrowManager.matches) rail.setAttribute("aria-hidden", "true");
    restoreFocus(rail);
    removeTransientLayer(
      backdrop,
      rail,
      "is-manager-visible",
      function () {
        if (managerTransitionSequence !== closeSequence) return;
        rail.classList.remove("is-manager-open");
        if (backdrop) backdrop.classList.remove("is-open");
        if (!narrowManager.matches) setRailDesktopSemantics(rail);
        updateDialogLock();
      },
      false
    );
  }

  function openDatasetManager() {
    var rail = document.querySelector(".rail");
    var summary = document.querySelector(".rail-summary");
    var backdrop = document.querySelector(".rail-manager-backdrop");
    if (!rail || !summary || !narrowManager.matches) return;
    managerTransitionSequence += 1;
    rail.classList.add("is-manager-open");
    if (backdrop) backdrop.classList.add("is-open");
    rail.removeAttribute("aria-hidden");
    summary.setAttribute("aria-expanded", "true");
    prepareDialog(rail, summary, closeDatasetManager);
    showTransientLayer(backdrop, rail, "is-manager-visible");
  }

  function applyRailMode() {
    var rail = document.querySelector(".rail");
    if (!rail) return;
    if (narrowManager.matches) {
      if (!rail.classList.contains("is-manager-open")) {
        rail.setAttribute("aria-hidden", "true");
      }
    } else {
      managerTransitionSequence += 1;
      rail.classList.remove("is-manager-open");
      rail.classList.remove("is-manager-visible");
      var backdrop = document.querySelector(".rail-manager-backdrop");
      if (backdrop) {
        var state = backdrop.__builderTransientState;
        if (state && state.cancel) state.cancel();
        delete backdrop.__builderTransientState;
        backdrop.classList.remove("is-open", "is-visible");
      }
      var summary = document.querySelector(".rail-summary");
      if (summary) summary.setAttribute("aria-expanded", "false");
      rail.removeEventListener("keydown", trapDialogKeydown);
      setRailDesktopSemantics(rail);
      updateDialogLock();
    }
  }

  function setupRail() {
    var rail = document.querySelector(".rail");
    if (!rail || rail.dataset.builderManager === "true") return;
    rail.dataset.builderManager = "true";
    rail.id = rail.id || "builder-dataset-manager";
    setRailDesktopSemantics(rail);

    var summary = document.createElement("button");
    summary.type = "button";
    summary.className = "rail-summary";
    summary.setAttribute("aria-controls", rail.id);
    summary.setAttribute("aria-expanded", "false");
    var text = document.createElement("span");
    text.className = "rail-summary-copy";
    var name = document.createElement("span");
    name.className = "rail-summary-name";
    var state = document.createElement("span");
    state.className = "rail-summary-state";
    text.appendChild(name);
    text.appendChild(state);
    var affordance = document.createElement("span");
    affordance.className = "rail-summary-action";
    affordance.textContent = "Dataset Manager";
    summary.appendChild(text);
    summary.appendChild(affordance);
    rail.parentNode.insertBefore(summary, rail);

    var backdrop = document.createElement("button");
    backdrop.type = "button";
    backdrop.className = "rail-manager-backdrop";
    backdrop.setAttribute("aria-label", "Close Dataset Manager");
    rail.parentNode.insertBefore(backdrop, rail);

    var close = document.createElement("button");
    close.type = "button";
    close.className = "rail-manager-close";
    close.setAttribute("aria-label", "Close Dataset Manager");
    close.textContent = "Close";
    rail.querySelector(".rail-head").appendChild(close);
    updateRailSummary();
    applyRailMode();
  }

  function showRemoveConfirmation(removeDataset) {
    var backdrop = document.createElement("div");
    backdrop.className = "builder-confirm-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-confirm-dialog";
    var title = document.createElement("h2");
    title.id = "builder-remove-title";
    title.textContent = "Remove dataset setup?";
    var message = document.createElement("p");
    message.textContent = "You can undo the most recent removal in this session.";
    var actions = document.createElement("div");
    actions.className = "builder-dialog-actions builder-confirm-actions";
    var cancel = document.createElement("button");
    cancel.type = "button";
    cancel.className = "btn";
    cancel.textContent = "Keep dataset";
    var confirm = document.createElement("button");
    confirm.type = "button";
    confirm.className = "btn btn-remove-soft";
    confirm.textContent = "Remove dataset";
    actions.appendChild(cancel);
    actions.appendChild(confirm);
    dialog.appendChild(title);
    dialog.appendChild(message);
    dialog.appendChild(actions);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);

    var closed = false;
    function close(commitRemoval) {
      if (closed) return;
      closed = true;
      if (commitRemoval) {
        send("drop_ds", { id: removeDataset.dataset.ds, confirmed: true });
      }
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
      });
    }
    cancel.addEventListener("click", function () { close(false); });
    confirm.addEventListener("click", function () {
      close(true);
    });
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close();
    });
    prepareDialog(dialog, removeDataset, close, removeDatasetFocusFallback);
    showTransientLayer(backdrop, dialog);
  }

  function datasetRailFocusIdentity(element) {
    var row = element && element.closest && element.closest(".ds[data-ds]");
    if (!row) return null;
    var action = element.closest(".builder-pick, .builder-reorder, .builder-drop");
    if (!action) return null;
    return {
      datasetId: row.dataset.ds,
      kind: action.classList.contains("builder-pick") ? "pick" :
        action.classList.contains("builder-reorder") ? "reorder" : "drop",
      direction: action.dataset.direction || null,
    };
  }

  function datasetRailFocusTarget(rail, identity) {
    if (!identity) return null;
    var row = Array.from(rail.querySelectorAll(".ds[data-ds]")).find(function (item) {
      return item.dataset.ds === identity.datasetId;
    });
    if (!row) return null;
    if (identity.kind === "pick") return row.querySelector(".builder-pick");
    if (identity.kind === "drop") return row.querySelector(".builder-drop");
    return Array.from(row.querySelectorAll(".builder-reorder")).find(function (item) {
      return item.dataset.direction === identity.direction;
    }) || null;
  }

  function parseDatasetRailElement(html, expectedId, expectedFingerprint) {
    if (typeof html !== "string" || !html.trim()) return null;
    var template = document.createElement("template");
    template.innerHTML = html.trim();
    if (template.content.childElementCount !== 1) return null;
    var row = template.content.firstElementChild;
    if (
      !row.matches(".ds.ds--ready[data-ds]") ||
      row.dataset.ds !== expectedId ||
      row.dataset.railFingerprint !== expectedFingerprint
    ) return null;
    return row;
  }

  function removeReadyImportOverlap() {
    var readyIds = new Set(Array.from(
      document.querySelectorAll("#ds_ready_list > .ds[data-ds]"),
      function (row) { return row.dataset.ds; }
    ));
    var host = document.getElementById("ds_import_list");
    if (!host || !readyIds.size) return;
    host.querySelectorAll(".ds[data-import-id]").forEach(function (row) {
      if (readyIds.has(row.dataset.importId)) row.remove();
    });
    var list = host.querySelector(":scope > .builder-import-list");
    if (list && !list.querySelector(".ds[data-import-id]")) {
      host.replaceChildren();
    }
  }

  function reconcileDatasetRail(message) {
    var rail = document.getElementById("ds_ready_list");
    var rows = message && message.rows;
    if (!rail || !Array.isArray(rows) || typeof message.empty_html !== "string") return;

    var existing = new Map();
    rail.querySelectorAll(":scope > .ds[data-ds]").forEach(function (row) {
      existing.set(row.dataset.ds, row);
    });
    var ids = new Set();
    var target = [];
    for (var index = 0; index < rows.length; index++) {
      var record = rows[index];
      if (
        !record || typeof record.id !== "string" || !record.id ||
        typeof record.fingerprint !== "string" || ids.has(record.id)
      ) {
        console.error("Builder rejected an invalid dataset rail snapshot.");
        return;
      }
      var currentRow = existing.get(record.id);
      var parsed = null;
      var needsMarkup = (
        !currentRow ||
        currentRow.dataset.railFingerprint !== record.fingerprint
      );
      if (needsMarkup) {
        parsed = parseDatasetRailElement(
          record.html, record.id, record.fingerprint
        );
      }
      if (needsMarkup && !parsed) {
        console.error("Builder rejected invalid dataset rail row markup.");
        send("builder_dataset_rail_sync", { nonce: Date.now() });
        return;
      }
      ids.add(record.id);
      target.push({ record: record, parsed: parsed });
    }

    var empty = null;
    if (!target.length) {
      var emptyTemplate = document.createElement("template");
      emptyTemplate.innerHTML = message.empty_html.trim();
      empty = emptyTemplate.content.childElementCount === 1 ?
        emptyTemplate.content.firstElementChild : null;
      if (!empty || !empty.matches(".rail-empty")) {
        console.error("Builder rejected invalid empty dataset rail markup.");
        return;
      }
    }

    var focusedElement = document.activeElement;
    var focusIdentity = datasetRailFocusIdentity(focusedElement);

    if (!target.length) {
      rail.replaceChildren(empty);
    } else {
      rail.querySelectorAll(":scope > .rail-empty").forEach(function (node) {
        node.remove();
      });
      target.forEach(function (item) {
        var row = existing.get(item.record.id);
        if (!row || row.dataset.railFingerprint !== item.record.fingerprint) {
          if (row) row.replaceWith(item.parsed);
          row = item.parsed;
        }
        rail.appendChild(row);
        existing.delete(item.record.id);
      });
      existing.forEach(function (row) { row.remove(); });
    }

    removeReadyImportOverlap();

    var nextFocus = datasetRailFocusTarget(rail, focusIdentity);
    if (nextFocus && focusedElement !== nextFocus) nextFocus.focus();
    document.querySelectorAll(".builder-confirm-dialog").forEach(function (dialog) {
      var identity = datasetRailFocusIdentity(dialog.__builderRestoreFocus);
      var replacement = datasetRailFocusTarget(rail, identity);
      if (replacement) dialog.__builderRestoreFocus = replacement;
    });
    updateRailSummary();
    if (window.BuilderIcons) window.BuilderIcons.decorate(rail);
    applyDatasetMutationLock([rail]);
    rail.querySelectorAll(".builder-pick").forEach(function (control) {
      setActivityDisabled(control, !activityCapability("select_dataset"));
    });
    updateDatasetLoadTimes([rail]);
    scheduleDatasetLoadTimeUpdates();
    if (datasetSwitchState.target) {
      var authoritative = rail.querySelector(
        ".builder-pick[aria-current=true]"
      );
      if (
        authoritative &&
        authoritative.dataset.ds !== datasetSwitchState.target
      ) {
        datasetSwitchState.authoritative = authoritative.dataset.ds;
        settleDatasetSwitch("error");
      } else {
        ensureDatasetSwitchVeil();
      }
    }
  }

  function importRailFocusIdentity(element) {
    var row = element && element.closest && element.closest(".ds[data-import-id]");
    var action = element && element.closest && element.closest(
      ".builder-pick-import, .builder-retry-import, .builder-remove-import"
    );
    if (!row || !action) return null;
    return {
      importId: row.dataset.importId,
      action: action.classList.contains("builder-pick-import") ? "pick" :
        action.classList.contains("builder-retry-import") ? "retry" : "remove",
    };
  }

  function importRailFocusTarget(rail, identity) {
    if (!identity) return null;
    var row = Array.from(rail.querySelectorAll(".ds[data-import-id]")).find(
      function (item) { return item.dataset.importId === identity.importId; }
    );
    if (!row) return null;
    var selector = identity.action === "pick" ? ".builder-pick-import" :
      identity.action === "retry" ? ".builder-retry-import" :
        ".builder-remove-import";
    return row.querySelector(selector);
  }

  function parseImportRailElement(html, expectedId, expectedFingerprint) {
    if (typeof html !== "string" || !html.trim()) return null;
    var template = document.createElement("template");
    template.innerHTML = html.trim();
    if (template.content.childElementCount !== 1) return null;
    var row = template.content.firstElementChild;
    if (
      !row.matches(".ds.ds--import[data-import-id]") ||
      row.dataset.importId !== expectedId ||
      row.dataset.importFingerprint !== expectedFingerprint
    ) return null;
    return row;
  }

  function reconcileImportRail(message) {
    var host = document.getElementById("ds_import_list");
    var rows = message && message.rows;
    if (!host || !Array.isArray(rows)) return;

    var ids = new Set();
    var target = [];
    for (var index = 0; index < rows.length; index++) {
      var record = rows[index];
      if (
        !record || typeof record.id !== "string" || !record.id ||
        typeof record.fingerprint !== "string" || ids.has(record.id)
      ) {
        console.error("Builder rejected an invalid import rail snapshot.");
        return;
      }
      var parsed = parseImportRailElement(
        record.html, record.id, record.fingerprint
      );
      if (!parsed) {
        console.error("Builder rejected invalid import rail row markup.");
        return;
      }
      ids.add(record.id);
      target.push({ record: record, parsed: parsed });
    }

    var focused = document.activeElement;
    var focusIdentity = importRailFocusIdentity(focused);
    var list = host.querySelector(":scope > .builder-import-list");
    if (!target.length) {
      host.replaceChildren();
      updateRailSummary();
      return;
    }
    if (!list) {
      list = document.createElement("div");
      list.className = "builder-import-list";
      list.setAttribute("aria-label", "Datasets being added");
      host.replaceChildren(list);
    }
    var existing = new Map();
    list.querySelectorAll(":scope > .ds[data-import-id]").forEach(function (row) {
      existing.set(row.dataset.importId, row);
    });
    target.forEach(function (item) {
      var row = existing.get(item.record.id);
      if (!row || row.dataset.importFingerprint !== item.record.fingerprint) {
        if (row) row.replaceWith(item.parsed);
        row = item.parsed;
      }
      list.appendChild(row);
      existing.delete(item.record.id);
    });
    existing.forEach(function (row) { row.remove(); });
    removeReadyImportOverlap();
    var nextFocus = importRailFocusTarget(list, focusIdentity);
    if (nextFocus && nextFocus !== focused) nextFocus.focus();
    if (window.BuilderIcons) window.BuilderIcons.decorate(host);
    applyDatasetMutationLock([host]);
    updateRailSummary();
  }

  function showAnalysisInfo(infoButton) {
    if (document.querySelector(".builder-analysis-info-backdrop")) return;

    var backdrop = document.createElement("div");
    backdrop.className = "builder-analysis-info-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-analysis-info-dialog";
    var header = document.createElement("div");
    header.className = "builder-analysis-info-header";
    var title = document.createElement("h2");
    title.id = "builder-analysis-info-title";
    title.textContent = infoButton.dataset.title || "Analysis details";
    var closeButton = document.createElement("button");
    closeButton.type = "button";
    closeButton.className = "builder-analysis-info-close";
    closeButton.setAttribute("aria-label", "Close analysis information");
    closeButton.textContent = "Close";
    header.appendChild(title);
    header.appendChild(closeButton);

    var description = document.createElement("p");
    description.id = "builder-analysis-info-description";
    description.className = "builder-analysis-info-description";
    description.textContent = infoButton.dataset.description || "";

    var facts = document.createElement("dl");
    facts.className = "builder-analysis-info-facts";
    [
      { label: "Available in", value: infoButton.dataset.pages },
      { label: "Typical time", value: infoButton.dataset.cost },
      { label: "Requires", value: infoButton.dataset.prerequisite },
      { label: "Network", value: infoButton.dataset.network },
      {
        label: "If already present",
        value: infoButton.dataset.replacement,
        wide: true,
      },
      { label: "If skipped", value: infoButton.dataset.skip, wide: true },
    ].forEach(function (fact) {
      var item = document.createElement("div");
      item.className = "builder-analysis-info-fact" +
        (fact.wide ? " is-wide" : "");
      var label = document.createElement("dt");
      label.textContent = fact.label;
      var value = document.createElement("dd");
      value.textContent = fact.value || "Not specified.";
      item.appendChild(label);
      item.appendChild(value);
      facts.appendChild(item);
    });

    dialog.appendChild(header);
    dialog.appendChild(description);
    dialog.appendChild(facts);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);
    dialog.setAttribute("aria-describedby", description.id);

    var closed = false;
    function close() {
      if (closed) return;
      closed = true;
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
      });
    }
    closeButton.addEventListener("click", close);
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close();
    });
    prepareDialog(dialog, infoButton, close);
    showTransientLayer(backdrop, dialog);
  }

  function setMarkerDialog(message) {
    var backdrop = document.getElementById("builder-marker-dialog-backdrop");
    var dialog = document.getElementById("builder-marker-dialog");
    var title = document.getElementById("builder-marker-dialog-title");
    var closeButton = document.getElementById("builder-marker-dialog-close");
    if (!backdrop || !dialog || !closeButton) return;

    function close() {
      if (backdrop.hidden) return;
      dialog.removeEventListener("keydown", trapDialogKeydown);
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        backdrop.hidden = true;
        updateDialogLock();
        restoreFocus(dialog);
      }, false);
    }

    if (!message || message.action === "close") {
      close();
      return;
    }
    if (title && message.title) title.textContent = message.title;
    backdrop.hidden = false;
    closeButton.onclick = close;
    backdrop.onclick = function (event) {
      if (event.target === backdrop) close();
    };
    dialog.removeEventListener("keydown", trapDialogKeydown);
    prepareDialog(
      dialog,
      document.querySelector(".marker-genes-action"),
      close
    );
    showTransientLayer(backdrop, dialog);
  }

  function showBuildDialog(message) {
    var existing = document.querySelector(".builder-build-dialog-backdrop");
    if (message && message.action === "close") {
      if (existing) existing.remove();
      updateDialogLock();
      return;
    }
    if (!message || message.type !== "conflict") return;
    if (existing) return;
    var trigger = document.getElementById("build");
    var backdrop = document.createElement("div");
    backdrop.className = "builder-confirm-backdrop builder-build-dialog-backdrop";
    var dialog = document.createElement("div");
    dialog.className = "builder-dialog builder-confirm-dialog builder-build-dialog";
    var title = document.createElement("h2");
    title.id = "builder-build-dialog-title";
    title.textContent = message.title || "Files already exist";
    dialog.appendChild(title);

    var description = document.createElement("p");
    description.textContent = "Some outputs already exist in this folder:";
    dialog.appendChild(description);

    var list = document.createElement("ul");
    list.className = "builder-build-dialog-list";
    var values = message.files || [];
    var shown = values.slice(0, 4);
    shown.forEach(function (name) {
      var item = document.createElement("li");
      item.textContent = name;
      list.appendChild(item);
    });
    if (values.length > shown.length) {
      var more = document.createElement("li");
      more.textContent = "…and " + (values.length - shown.length) + " more";
      list.appendChild(more);
    }
    dialog.appendChild(list);
    var question = document.createElement("p");
    question.textContent = "What would you like to do?";
    dialog.appendChild(question);

    var actions = document.createElement("div");
    actions.className = "builder-dialog-actions builder-confirm-actions builder-build-dialog-actions";
    var buttons = [
      { label: "Cancel", action: "cancel", className: "btn" },
      { label: "Replace existing files", action: "replace", className: "btn btn-replace" },
      { label: "Choose another folder", action: "choose_another", className: "btn btn-action" },
    ];
    var closed = false;
    function close(action) {
      if (closed) return;
      closed = true;
      send("builder_build_dialog", {
        action: action || "cancel",
        nonce: message.nonce,
      });
      removeTransientLayer(backdrop, dialog, "is-visible", function () {
        updateDialogLock();
        restoreFocus(dialog);
      });
    }
    buttons.forEach(function (definition) {
      var button = document.createElement("button");
      button.type = "button";
      button.className = definition.className;
      button.textContent = definition.label;
      button.addEventListener("click", function () { close(definition.action); });
      actions.appendChild(button);
    });
    dialog.appendChild(actions);
    backdrop.appendChild(dialog);
    document.body.appendChild(backdrop);
    dialog.setAttribute("aria-labelledby", title.id);
    backdrop.addEventListener("click", function (event) {
      if (event.target === backdrop) close("cancel");
    });
    prepareDialog(dialog, trigger, close);
    showTransientLayer(backdrop, dialog);
  }

  function setCurrentStage(stage) {
    document.querySelectorAll(".builder-stage").forEach(function (candidate) {
      candidate.removeAttribute("aria-current");
    });
    if (stage) stage.setAttribute("aria-current", "stage");
  }

  var stageObserver = new IntersectionObserver(
    function (entries) {
      entries.forEach(function (entry) {
        if (entry.target.isConnected) return;
        stageObserver.unobserve(entry.target);
        observedStages.delete(entry.target);
      });
      var visible = entries
        .filter(function (entry) {
          return entry.target.isConnected && entry.isIntersecting;
        })
        .sort(function (left, right) {
          return right.intersectionRatio - left.intersectionRatio;
        });
      if (visible.length) setCurrentStage(visible[0].target);
    },
    { rootMargin: "-15% 0px -55% 0px", threshold: [0.15, 0.5, 0.85] }
  );

  function registerStages(roots) {
    observedStages.forEach(function (stage) {
      if (stage.isConnected) return;
      stageObserver.unobserve(stage);
      observedStages.delete(stage);
    });
    var stages = matchingDynamicElements(roots, ".builder-stage");
    stages.forEach(function (stage) {
      if (stage.dataset.builderStage === "true") return;
      stage.dataset.builderStage = "true";
      stage.setAttribute("role", "region");
      var heading = stage.querySelector("h2");
      if (heading) {
        heading.id = heading.id || stage.id + "-heading";
        stage.setAttribute("aria-labelledby", heading.id);
      }
      stage.addEventListener("focusin", function () {
        setCurrentStage(stage);
      });
      stageObserver.observe(stage);
      observedStages.add(stage);
    });
    if (stages.length && !document.querySelector('[aria-current="stage"]')) {
      setCurrentStage(stages[0]);
    }
  }

  function ensureLiveRegion() {
    var live = document.getElementById("builder-live-status");
    if (live) return live;
    live = document.createElement("div");
    live.id = "builder-live-status";
    live.className = "visually-hidden";
    live.setAttribute("role", "status");
    live.setAttribute("aria-live", "polite");
    live.setAttribute("aria-atomic", "true");
    document.body.appendChild(live);
    return live;
  }

  function scheduleStatusAnnouncement(text) {
    var next = String(text || "").replace(/\s+/g, " ").trim();
    if (!next || next === lastAnnouncement) return;
    window.clearTimeout(statusTimer);
    statusTimer = window.setTimeout(function () {
      ensureLiveRegion().textContent = next;
      lastAnnouncement = next;
    }, 350);
  }

  function updateStatusSemantics() {
    ["busy", "result_card", "review_action_summary"].forEach(function (id) {
      var output = document.getElementById(id);
      if (!output) return;
      output.setAttribute("role", "status");
      output.setAttribute("aria-live", "polite");
      output.setAttribute("aria-atomic", "true");
    });
    var status = ["#busy", "#result_card", "#review_action_summary"]
      .map(function (selector) {
        var node = document.querySelector(selector);
        return node ? node.textContent : "";
      })
      .filter(Boolean)
      .join(". ");
    scheduleStatusAnnouncement(status);
  }

  function updatePipelines(roots) {
    var order = ["queued", "building", "complete"];
    matchingDynamicElements(roots, ".pipeline[data-pipeline-state]").forEach(function (pipeline) {
      var state = pipeline.dataset.pipelineState;
      var activeIndex = order.indexOf(state);
      pipeline.querySelectorAll(".pipeline-step").forEach(function (step) {
        var index = order.indexOf(step.dataset.step);
        var terminal = state === "complete" && step.dataset.step === "complete";
        var current = step.dataset.step === state && !terminal;
        var complete = state !== "failure" && index >= 0 &&
          (index < activeIndex || terminal);
        step.classList.toggle("is-complete", complete);
        step.classList.toggle("is-current", current);
        step.classList.toggle("is-terminal", terminal);
        if (current) {
          step.setAttribute("aria-current", "step");
        } else {
          step.removeAttribute("aria-current");
        }
      });
    });
  }

  var primaryObserver = new IntersectionObserver(
    function (entries) {
      entries.forEach(function (entry) {
        if (entry.target === observedPrimaryAction) {
          window.__builderPrimaryActionVisible =
            entry.isIntersecting && entry.intersectionRatio >= 0.99;
        }
      });
    },
    { threshold: [0, 0.99, 1] }
  );

  function registerPrimaryAction() {
    var action = document.getElementById("build");
    if (action === observedPrimaryAction) return;
    if (observedPrimaryAction) primaryObserver.unobserve(observedPrimaryAction);
    observedPrimaryAction = action;
    window.__builderPrimaryActionVisible = false;
    if (action) primaryObserver.observe(action);
  }

  function updateMotionDuration() {
    if (reducedMotion.matches) {
      window.__builderMotionDuration = 0;
      return;
    }
    var duration = getComputedStyle(document.documentElement)
      .getPropertyValue("--duration-normal")
      .trim();
    var match = duration.match(/^((?:\d+(?:\.\d+)?|\.\d+))(ms|s)$/);
    if (!match) {
      window.__builderMotionDuration = normalMotionDuration;
      return;
    }
    var value = Number(match[1]);
    var milliseconds = match[2] === "ms" ? value : value * 1000;
    window.__builderMotionDuration = Number.isFinite(milliseconds) ?
      Math.round(milliseconds) : normalMotionDuration;
  }


  function colourLabel(input) {
    var swatch = input.closest(".swatch");
    var name = swatch && swatch.querySelector(".swatch-name");
    var level = name ? name.textContent.trim() : input.dataset.level || "Colour";
    return level + ": " + input.value.toUpperCase();
  }

  function enhanceColour(input) {
    if (!input.hasAttribute("aria-label")) {
      input.setAttribute("aria-label", colourLabel(input));
    }
  }

  function updateGroupColor(input) {
    var item = input.closest(".group-color-item");
    var editor = input.closest(".builder-group-colors");
    var normalized = input.value.toUpperCase();
    var hex = item && item.querySelector(".group-color-hex");
    var status = editor && editor.querySelector(".group-color-status");
    var label = item && item.querySelector(".group-color-name");
    if (hex) hex.textContent = normalized;
    if (status) {
      status.textContent = "Color for " +
        (label ? label.textContent.trim() : input.dataset.level) +
        " changed to " + normalized + ".";
    }
    var projection = document.querySelector(".viewer-projection-workspace");
    if (projection && projection.dataset.previewGroup === input.dataset.group) {
      projection.querySelectorAll(".viewer-scatter-point[data-group]").forEach(
        function (point) {
          if (point.dataset.group === input.dataset.level) {
            point.setAttribute("fill", normalized);
          }
        }
      );
    }
    send(input.dataset.inputId, {
      group: input.dataset.group,
      level: input.dataset.level,
      color: normalized,
      nonce: Date.now(),
    });
  }

  function filterGroupColors(search) {
    var editor = search.closest(".builder-group-colors");
    if (!editor) return;
    var grid = editor.querySelector(".group-color-grid");
    var query = search.value.trim().toLowerCase();
    grid.classList.toggle("is-searching", query.length > 0);
    grid.querySelectorAll(".group-color-item").forEach(function (item) {
      item.hidden = query.length > 0 &&
        !item.dataset.search.includes(query);
    });
  }

  function toggleGroupColors(button) {
    var editor = button.closest(".builder-group-colors");
    if (!editor) return;
    var grid = editor.querySelector(".group-color-grid");
    var showAll = button.dataset.action === "show-all";
    grid.classList.toggle("is-expanded", showAll);
    grid.classList.toggle("is-collapsed", !showAll);
    editor.querySelector('[data-action="show-all"]').hidden = showAll;
    editor.querySelector('[data-action="show-fewer"]').hidden = !showAll;
  }

  function viewerGroupRows(root) {
    return Array.from(root.querySelectorAll(".viewer-group-row"));
  }

  function updateViewerMetadataSelection(root, emit) {
    if (!root) return;
    var retained = [];
    viewerGroupRows(root).forEach(function (row) {
      var metadata = row.querySelector(".viewer-metadata-retain");
      var group = row.querySelector(".viewer-group-include");
      var radio = row.querySelector(".viewer-group-default");
      if (metadata && metadata.checked) retained.push(row.dataset.group);
      if (metadata && !metadata.checked) {
        if (group) group.checked = false;
        if (radio) radio.checked = false;
      }
    });
    updateViewerGroupSelection(root, false, true);
    if (emit && root.dataset.metadataInputId) {
      send(root.dataset.metadataInputId, {
        action: "set-retention",
        retained: retained,
        nonce: Date.now(),
      });
    }
  }

  function selectViewerMetadata(button) {
    var root = button.closest(".viewer-group-workspace");
    if (!root) return;
    var action = button.dataset.action;
    viewerGroupRows(root).forEach(function (row) {
      var checkbox = row.querySelector(".viewer-metadata-retain");
      if (!checkbox || checkbox.disabled) return;
      checkbox.checked = action === "all-supported" ||
        row.dataset.recommendedRetained === "true";
    });
    updateViewerMetadataSelection(root, true);
  }

  function updateDefaultCopy(root, selector) {
    root.querySelectorAll(selector).forEach(function (input) {
      var label = input.closest("label");
      var copy = label && label.querySelector(".viewer-default-copy");
      if (copy) {
        copy.textContent = input.checked
          ? "Default"
          : selector === ".viewer-group-default"
            ? "Set as default"
            : "Set default";
      }
    });
  }

  function updateDisclosureSummary(output, text) {
    if (!output || output.textContent === text) return;
    output.textContent = text;
    output.classList.remove("is-updating");
    void output.offsetWidth;
    output.classList.add("is-updating");
  }

  function updateViewerGroupCount(root) {
    var count = root.querySelectorAll(".viewer-group-include:checked").length;
    var card = root.closest(".builder-viewer-card");
    var output = card && card.querySelector("[data-viewer-group-count]");
    var selected = root.querySelector(".viewer-group-default:checked");
    var row = selected && selected.closest(".viewer-group-row");
    var label = row && row.querySelector(".viewer-group-name");
    updateDisclosureSummary(
      output,
      count + " included · Default: " +
        (label ? label.textContent.trim() : "None")
    );
  }

  function updateViewerGroupSelection(root, emit, allowEmpty) {
    if (!root) return;
    var rows = viewerGroupRows(root);
    var included = rows
      .filter(function (row) {
        var checkbox = row.querySelector(".viewer-group-include");
        return checkbox && checkbox.checked && !checkbox.disabled;
      })
      .map(function (row) { return row.dataset.group; });
    if (!included.length && !allowEmpty) {
      var first = rows.find(function (row) {
        return row.dataset.eligible === "true";
      });
      var firstCheckbox = first && first.querySelector(".viewer-group-include");
      if (firstCheckbox) {
        firstCheckbox.checked = true;
        included = [first.dataset.group];
      }
    }
    var currentDefault = root.querySelector(".viewer-group-default:checked");
    var defaultGroup = currentDefault && included.includes(currentDefault.value)
      ? currentDefault.value
      : included[0] || null;
    rows.forEach(function (row) {
      var checkbox = row.querySelector(".viewer-group-include");
      var radio = row.querySelector(".viewer-group-default");
      var isIncluded = Boolean(checkbox && checkbox.checked && !checkbox.disabled);
      row.classList.toggle("is-included", isIncluded);
      if (radio) {
        radio.disabled = row.dataset.eligible !== "true";
        radio.checked = isIncluded && row.dataset.group === defaultGroup;
      }
    });
    updateDefaultCopy(root, ".viewer-group-default");
    updateViewerGroupCount(root);
    if (emit && root.dataset.inputId) {
      send(root.dataset.inputId, {
        action: "set-groups",
        included: included,
        default: defaultGroup,
        nonce: Date.now(),
      });
    }
  }

  function focusViewerGroup(button) {
    var root = button.closest(".viewer-group-workspace");
    if (!root) return;
    viewerGroupRows(root).forEach(function (row) {
      var focus = row.querySelector(".viewer-group-focus");
      var selected = row.dataset.group === button.dataset.group;
      row.classList.toggle("is-focused", selected);
      if (focus) focus.setAttribute("aria-pressed", selected ? "true" : "false");
    });
    if (root.dataset.focusInputId) {
      send(root.dataset.focusInputId, {
        group: button.dataset.group,
        nonce: Date.now(),
      });
    }
  }

  function filterViewerGroups(search) {
    var root = search.closest(".viewer-group-workspace");
    if (!root) return;
    var query = search.value.trim().toLowerCase();
    viewerGroupRows(root).forEach(function (row) {
      row.hidden = query.length > 0 && !row.dataset.search.includes(query);
    });
  }

  function selectViewerGroups(button) {
    var root = button.closest(".viewer-group-workspace");
    if (!root) return;
    var action = button.dataset.action;
    viewerGroupRows(root).forEach(function (row) {
      var checkbox = row.querySelector(".viewer-group-include");
      if (!checkbox || checkbox.disabled) return;
      checkbox.checked = action === "all" || row.dataset.suggested === "true";
    });
    updateViewerGroupSelection(root, true);
  }

  function setupViewerGroupCatalogs(roots) {
    matchingDynamicElements(roots, ".viewer-group-workspace").forEach(function (workspace) {
      if (workspace.dataset.builderGroups === "true") return;
      workspace.dataset.builderGroups = "true";
      updateViewerGroupSelection(workspace, false);
      var initial = workspace.querySelector(".viewer-group-default:checked");
      var row = initial && initial.closest(".viewer-group-row");
      var focus = row && row.querySelector(".viewer-group-focus");
      if (focus) {
        row.classList.add("is-focused");
        focus.setAttribute("aria-pressed", "true");
      }
    });
  }

  function applyViewerGroupState(message) {
    var root = document.querySelector(".viewer-group-workspace");
    if (!root) return;
    var included = new Set(messageValues(message && message.included));
    viewerGroupRows(root).forEach(function (row) {
      var checkbox = row.querySelector(".viewer-group-include");
      var radio = row.querySelector(".viewer-group-default");
      if (checkbox && !checkbox.disabled) checkbox.checked = included.has(row.dataset.group);
      if (radio) radio.checked = row.dataset.group === (message && message.default);
    });
    updateViewerGroupSelection(root, false);
    var status = root.querySelector(".viewer-group-status");
    if (status && message && message.message) status.textContent = message.message;
  }

  function projectionCards(root) {
    return Array.from(root.querySelectorAll(".viewer-projection-card"));
  }

  function updateProjectionSelection(root, emit) {
    if (!root) return;
    var cards = projectionCards(root);
    var included = cards
      .filter(function (card) {
        var checkbox = card.querySelector(".viewer-projection-include");
        return checkbox && checkbox.checked && !checkbox.disabled;
      })
      .map(function (card) { return card.dataset.projection; });
    if (!included.length) {
      var first = cards.find(function (card) {
        var checkbox = card.querySelector(".viewer-projection-include");
        return checkbox && !checkbox.disabled;
      });
      var firstCheckbox = first && first.querySelector(".viewer-projection-include");
      if (firstCheckbox) {
        firstCheckbox.checked = true;
        included = [first.dataset.projection];
      }
    }
    var selectedDefault = root.querySelector(".viewer-projection-default:checked");
    var defaultProjection = selectedDefault && included.includes(selectedDefault.value)
      ? selectedDefault.value
      : included[0] || null;
    cards.forEach(function (card) {
      var checkbox = card.querySelector(".viewer-projection-include");
      var radio = card.querySelector(".viewer-projection-default");
      var selected = Boolean(checkbox && checkbox.checked && !checkbox.disabled);
      card.classList.toggle("is-included", selected);
      if (radio) {
        radio.disabled = !selected;
        radio.checked = selected && card.dataset.projection === defaultProjection;
      }
    });
    updateDefaultCopy(root, ".viewer-projection-default");
    var summary = root.closest(".builder-viewer-card");
    var countOutput = summary && summary.querySelector("[data-viewer-projection-count]");
    var defaultCard = cards.find(function (card) {
      return card.dataset.projection === defaultProjection;
    });
    var defaultLabel = defaultCard && defaultCard.querySelector("h4");
    updateDisclosureSummary(
      countOutput,
      included.length + " included · Default: " +
        (defaultLabel ? defaultLabel.textContent.trim() : "None")
    );
    if (emit && root.dataset.inputId) {
      send(root.dataset.inputId, {
        action: "set",
        included: included,
        default: defaultProjection,
        nonce: Date.now(),
      });
    }
  }

  function updateProjectionPointSize(input, emit) {
    var root = input.closest(".viewer-projection-workspace");
    if (!root) return;
    var value = Number(input.value);
    if (!Number.isFinite(value)) return;
    var radius = Math.max(0, Math.min(4.5, value * 0.34));
    var output = root.querySelector(".viewer-point-size-value");
    if (output) output.textContent = String(value);
    root.querySelectorAll(".viewer-projection-preview").forEach(function (svg) {
      svg.dataset.pointSize = String(value);
      svg.querySelectorAll(".viewer-scatter-point").forEach(function (point) {
        point.setAttribute("r", radius.toFixed(2));
      });
    });
    if (emit && input.dataset.inputId) send(input.dataset.inputId, value);
  }

  function updateProjectionCellPercentage(input, emit) {
    var root = input.closest(".viewer-projection-workspace");
    if (!root) return;
    var value = Number(input.value);
    if (!Number.isFinite(value)) return;
    var output = root.querySelector(".viewer-cell-percentage-value");
    if (output) output.textContent = String(value) + "%";
    if (emit && input.dataset.inputId) send(input.dataset.inputId, value);
  }

  function trajectoryCards(root) {
    return Array.from(root.querySelectorAll(".viewer-trajectory-card"));
  }

  function trajectoryRecord(card) {
    return { method: card.dataset.method, name: card.dataset.trajectory };
  }

  function updateTrajectorySelection(root, emit) {
    if (!root) return;
    var cards = trajectoryCards(root);
    var includedCards = cards.filter(function (card) {
      var checkbox = card.querySelector(".viewer-trajectory-include");
      return checkbox && checkbox.checked && !checkbox.disabled;
    });
    var selectedDefault = root.querySelector(".viewer-trajectory-default:checked");
    var defaultCard = selectedDefault && selectedDefault.closest(".viewer-trajectory-card");
    if (!defaultCard || !includedCards.includes(defaultCard)) {
      defaultCard = includedCards[0] || null;
    }
    cards.forEach(function (card) {
      var checkbox = card.querySelector(".viewer-trajectory-include");
      var radio = card.querySelector(".viewer-trajectory-default");
      var selected = Boolean(checkbox && checkbox.checked && !checkbox.disabled);
      card.classList.toggle("is-included", selected);
      if (radio) {
        radio.disabled = !selected;
        radio.checked = selected && card === defaultCard;
      }
    });
    updateDefaultCopy(root, ".viewer-trajectory-default");
    var summary = root.closest(".builder-viewer-card");
    var countOutput = summary && summary.querySelector("[data-viewer-trajectory-count]");
    var defaultLabel = defaultCard && defaultCard.querySelector("h4");
    updateDisclosureSummary(
      countOutput,
      includedCards.length + " included" +
        (defaultLabel ? " · Default: " + defaultLabel.textContent.trim() : "")
    );
    if (emit && root.dataset.inputId) {
      send(root.dataset.inputId, {
        action: "set",
        included: includedCards.map(trajectoryRecord),
        default: defaultCard ? trajectoryRecord(defaultCard) : null,
        nonce: Date.now(),
      });
    }
  }

  function setupViewerContentCatalogs(roots) {
    matchingDynamicElements(roots, ".viewer-projection-workspace").forEach(function (workspace) {
      if (workspace.dataset.builderProjections === "true") return;
      workspace.dataset.builderProjections = "true";
      updateProjectionSelection(workspace, false);
      var pointSize = workspace.querySelector(".viewer-point-size-input");
      if (pointSize) updateProjectionPointSize(pointSize, false);
      var cellPercentage = workspace.querySelector(".viewer-cell-percentage-input");
      if (cellPercentage) updateProjectionCellPercentage(cellPercentage, false);
    });
    matchingDynamicElements(roots, ".viewer-trajectory-workspace").forEach(function (workspace) {
      if (workspace.dataset.builderTrajectories === "true") return;
      workspace.dataset.builderTrajectories = "true";
      updateTrajectorySelection(workspace, false);
    });
  }

  function disclosureStateKey(details) {
    var stage = details.closest(".builder-stage-core");
    var dataset = stage && stage.querySelector(".builder-rendered-for-input");
    var datasetId = dataset && dataset.value ? dataset.value : "builder";
    return datasetId + "::" + details.dataset.disclosureKey;
  }

  function setupPersistentDisclosures(roots) {
    matchingDynamicElements(roots, "details[data-disclosure-key]").forEach(function (details) {
      if (details.dataset.builderDisclosure === "true") return;
      var key = disclosureStateKey(details);
      if (viewerDisclosureState.has(key)) {
        details.open = viewerDisclosureState.get(key);
      }
      details.dataset.builderDisclosure = "true";
      details.addEventListener("toggle", function () {
        viewerDisclosureState.set(key, details.open);
      });
    });
  }

  function setupViewerContentAccordions(roots) {
    matchingDynamicElements(roots, ".builder-stage-configure").forEach(function (stage) {
      if (stage.dataset.builderAccordion === "true") return;
      stage.dataset.builderAccordion = "true";
      var cards = Array.from(stage.querySelectorAll(
        ".builder-viewer-card[data-disclosure-key]"
      ));
      cards.forEach(function (card) {
        card.addEventListener("toggle", function () {
          if (!card.open) return;
          cards.forEach(function (sibling) {
            if (sibling !== card) sibling.open = false;
          });
        });
      });
    });
  }

  function applyViewerProjectionState(message) {
    var root = document.querySelector(".viewer-projection-workspace");
    if (!root) return;
    var included = new Set(messageValues(message && message.included));
    projectionCards(root).forEach(function (card) {
      var checkbox = card.querySelector(".viewer-projection-include");
      var radio = card.querySelector(".viewer-projection-default");
      if (checkbox && !checkbox.disabled) checkbox.checked = included.has(card.dataset.projection);
      if (radio) radio.checked = card.dataset.projection === (message && message.default);
    });
    var pointSize = root.querySelector(".viewer-point-size-input");
    if (pointSize && message && Number.isFinite(Number(message.point_size))) {
      pointSize.value = String(message.point_size);
      updateProjectionPointSize(pointSize, false);
    }
    var cellPercentage = root.querySelector(".viewer-cell-percentage-input");
    if (
      cellPercentage &&
      message &&
      Number.isFinite(Number(message.percentage_cells_to_show))
    ) {
      cellPercentage.value = String(message.percentage_cells_to_show);
      updateProjectionCellPercentage(cellPercentage, false);
    }
    updateProjectionSelection(root, false);
    var status = root.querySelector(".viewer-projection-status");
    if (status && message && message.message) status.textContent = message.message;
  }

  function applyViewerTrajectoryState(message) {
    var root = document.querySelector(".viewer-trajectory-workspace");
    if (!root) return;
    var included = new Set(messageValues(message && message.included).map(function (record) {
      return record.method + "::" + record.name;
    }));
    var defaultKey = message && message.default
      ? message.default.method + "::" + message.default.name
      : null;
    trajectoryCards(root).forEach(function (card) {
      var checkbox = card.querySelector(".viewer-trajectory-include");
      var radio = card.querySelector(".viewer-trajectory-default");
      if (checkbox && !checkbox.disabled) checkbox.checked = included.has(card.dataset.trajectoryKey);
      if (radio) radio.checked = card.dataset.trajectoryKey === defaultKey;
    });
    updateTrajectorySelection(root, false);
    var status = root.querySelector(".viewer-trajectory-status");
    if (status && message && message.message) status.textContent = message.message;
  }

  function normalizeCreatableSelectValue(value) {
    return String(value || "").trim().replace(/\s+/g, " ");
  }

  function setupCreatableSelect(root) {
    if (!root || root.dataset.builderCreatableSelectReady === "true") return;
    var select = root.querySelector("select");
    var selectize = select && select.selectize;
    var dropdown = selectize && selectize.$dropdown && selectize.$dropdown[0];
    if (!selectize || !dropdown) return;

    var inputLabel = root.dataset.builderCreateInputLabel || "Custom value";
    var actionLabel = root.dataset.builderCreateActionLabel || "Add custom value";
    var maximumLength = parseInt(root.dataset.builderCreateMaxlength, 10) || 80;
    var row = document.createElement("div");
    row.className = "builder-creatable-select-row";
    var input = document.createElement("input");
    input.type = "text";
    input.className = "builder-creatable-select-input";
    input.placeholder = root.dataset.builderCreatePlaceholder || "Type another value";
    input.setAttribute("aria-label", inputLabel);
    input.setAttribute("autocomplete", "off");
    var add = document.createElement("button");
    add.type = "button";
    add.className = "builder-creatable-select-add";
    add.textContent = "Add";
    add.setAttribute("aria-label", actionLabel);
    add.disabled = true;
    var error = document.createElement("p");
    error.className = "builder-creatable-select-error";
    error.setAttribute("aria-live", "polite");
    error.hidden = true;
    row.append(input, add, error);
    dropdown.appendChild(row);

    function updateState() {
      var value = normalizeCreatableSelectValue(input.value);
      var tooLong = value.length > maximumLength;
      add.disabled = !value || tooLong;
      error.hidden = !tooLong;
      error.textContent = tooLong ?
        "Use " + maximumLength + " characters or fewer." : "";
      input.setAttribute("aria-invalid", tooLong ? "true" : "false");
    }

    function matchingOption(value) {
      var comparison = value.toLocaleLowerCase();
      return Object.keys(selectize.options).find(function (key) {
        var option = selectize.options[key] || {};
        return [
          key,
          option[selectize.settings.valueField],
          option[selectize.settings.labelField],
        ].some(function (candidate) {
          return normalizeCreatableSelectValue(candidate).toLocaleLowerCase() === comparison;
        });
      });
    }

    function commitValue() {
      var value = normalizeCreatableSelectValue(input.value);
      if (!value || value.length > maximumLength) {
        updateState();
        return;
      }
      var existing = matchingOption(value);
      var selectedValue = existing || value;
      if (!existing) {
        var option = {};
        option[selectize.settings.valueField] = value;
        option[selectize.settings.labelField] = value;
        selectize.addOption(option);
      }
      selectize.setValue(selectedValue);
      selectize.close();
      input.value = "";
      updateState();
      window.setTimeout(function () {
        var controlInput = selectize.$control_input && selectize.$control_input[0];
        if (canRestoreFocus(controlInput)) controlInput.focus();
      }, 0);
    }

    row.addEventListener("mousedown", function (event) {
      event.stopPropagation();
    });
    input.addEventListener("mousedown", function () {
      selectize.ignoreFocus = true;
    });
    input.addEventListener("focus", function () {
      selectize.ignoreFocus = false;
      selectize.isFocused = true;
      selectize.isBlurring = false;
      selectize.$control.addClass("focus");
    });
    input.addEventListener("input", updateState);
    input.addEventListener("keydown", function (event) {
      if (event.key !== "Enter") return;
      event.preventDefault();
      event.stopPropagation();
      commitValue();
    });
    add.addEventListener("click", commitValue);
    root.dataset.builderCreatableSelectReady = "true";
  }

  function setupCreatableSelects(roots) {
    matchingDynamicElements(roots, "[data-builder-creatable-select='true']").forEach(
      setupCreatableSelect
    );
  }

  var multiSelectPlaceholder = "Select…";

  function minimumMultiSelectEmptyWidth() {
    var more = document.getElementById("cv-more-btn");
    return more ? Math.ceil(more.getBoundingClientRect().width) : 140;
  }

  function multiSelectEmptyWidth(select, instance) {
    var control = instance.$control && instance.$control[0];
    var style = window.getComputedStyle(control || select);
    var canvas = multiSelectEmptyWidth.canvas ||
      (multiSelectEmptyWidth.canvas = document.createElement("canvas"));
    var context = canvas.getContext("2d");
    context.font = style.font || [style.fontSize, style.fontFamily].join(" ");
    var longest = Array.prototype.reduce.call(select.options, function (width, option) {
      return Math.max(width, context.measureText(option.textContent || "").width);
    }, 0);
    return Math.min(
      window.innerWidth - 32,
      Math.max(minimumMultiSelectEmptyWidth(), Math.ceil(longest + 42))
    );
  }

  function sizeEmptyMultiSelect(select, instance) {
    var empty = !instance.items.length;
    instance.$wrapper.toggleClass("cerebro-multiselect-empty", empty);
    instance.$wrapper.css(
      "width",
      empty ? multiSelectEmptyWidth(select, instance) + "px" : ""
    );
  }

  function enhanceMultiSelect(select) {
    if (!select || !select.multiple) return;
    select.setAttribute("data-placeholder", multiSelectPlaceholder);
    if (!select.selectize) return;
    select.selectize.settings.placeholder = multiSelectPlaceholder;
    select.selectize.$control_input.attr("placeholder", multiSelectPlaceholder);
    select.selectize.updatePlaceholder();
    select.selectize.$wrapper.addClass("cerebro-multiselect");
    sizeEmptyMultiSelect(select, select.selectize);
    if (!select.dataset.cerebroMultiSelectReady) {
      select.selectize.on("change", function () {
        sizeEmptyMultiSelect(select, select.selectize);
      });
      select.dataset.cerebroMultiSelectReady = "true";
    }
  }

  function spatialAlignmentHasImage(sidebar) {
    var imageControls = sidebar.querySelector(
      ".spatial-alignment-sidebar-scroll > .shiny-panel-conditional"
    );
    return Boolean(
      imageControls && window.getComputedStyle(imageControls).display !== "none"
    );
  }

  function syncSpatialAlignmentScrollbar(sidebar) {
    if (!sidebar.dataset.builderWheelPageScroll) {
      sidebar.dataset.builderWheelPageScroll = "true";
      sidebar.addEventListener("wheel", function (event) {
        if (event.ctrlKey) return;
        var multiplier = event.deltaMode === 1
          ? 16
          : event.deltaMode === 2
            ? window.innerHeight
            : 1;
        event.preventDefault();
        window.scrollBy(0, event.deltaY * multiplier);
      }, { passive: false });
    }
    var scrollbar = sidebar.querySelector(".spatial-alignment-persistent-scrollbar");
    if (!scrollbar) {
      scrollbar = document.createElement("div");
      scrollbar.className = "spatial-alignment-persistent-scrollbar";
      scrollbar.setAttribute("aria-hidden", "true");
      scrollbar.innerHTML = '<div class="spatial-alignment-persistent-scrollbar-thumb"></div>';
      sidebar.appendChild(scrollbar);

      scrollbar.addEventListener("pointerdown", function (event) {
        event.preventDefault();
        var start = event.clientY;
        var initialScroll = sidebar.scrollTop;
        var available = Math.max(1, sidebar.scrollHeight - sidebar.clientHeight);
        var thumb = scrollbar.firstElementChild;
        var track = Math.max(1, scrollbar.clientHeight - thumb.clientHeight);
        function drag(moveEvent) {
          sidebar.scrollTop = initialScroll +
            ((moveEvent.clientY - start) / track) * available;
        }
        function stop() {
          document.removeEventListener("pointermove", drag);
          document.removeEventListener("pointerup", stop);
        }
        document.addEventListener("pointermove", drag);
        document.addEventListener("pointerup", stop, { once: true });
      });
      sidebar.addEventListener("scroll", scheduleSpatialAlignmentScrollbars, {
        passive: true,
      });
    }

    var visible = spatialAlignmentHasImage(sidebar);
    scrollbar.hidden = !visible;
    if (!visible) return;

    var rect = sidebar.getBoundingClientRect();
    var trackHeight = Math.max(0, rect.height);
    var range = Math.max(0, sidebar.scrollHeight - sidebar.clientHeight);
    var thumbHeight = range > 0
      ? Math.max(28, trackHeight * (sidebar.clientHeight / sidebar.scrollHeight))
      : trackHeight;
    var travel = Math.max(0, trackHeight - thumbHeight);
    var thumbTop = range > 0 ? travel * (sidebar.scrollTop / range) : 0;
    var thumb = scrollbar.firstElementChild;

    scrollbar.style.top = rect.top + "px";
    scrollbar.style.left = (rect.right - 10) + "px";
    scrollbar.style.height = trackHeight + "px";
    thumb.style.height = thumbHeight + "px";
    thumb.style.transform = "translateY(" + thumbTop + "px)";
  }

  function syncSpatialAlignmentScrollbars(roots) {
    matchingDynamicElements(roots, ".spatial-alignment-sidebar").forEach(
      syncSpatialAlignmentScrollbar
    );
  }

  function scheduleSpatialAlignmentScrollbars() {
    if (spatialScrollbarFrame !== null) return;
    spatialScrollbarFrame = window.requestAnimationFrame(function () {
      spatialScrollbarFrame = null;
      syncSpatialAlignmentScrollbars();
    });
  }

  function updateOptionalAnalysisCount(roots) {
    matchingDynamicContainers(
      roots,
      ".builder-viewer-optional-analyses"
    ).forEach(function (disclosure) {
      var output = disclosure.querySelector("[data-analysis-count]");
      if (!output) return;
      var selected = disclosure.querySelectorAll(
        ".enhance-module-checkbox:checked, " +
        ".marker-genes-action[aria-pressed=\"true\"]"
      ).length;
      updateDisclosureSummary(output, selected + " included");
    });
  }

  function updateExtraMaterialCount(roots) {
    matchingDynamicContainers(
      roots,
      ".builder-viewer-extra-material"
    ).forEach(function (disclosure) {
      var output = disclosure.querySelector("[data-extra-material-count]");
      if (!output) return;
      updateDisclosureSummary(
        output,
        disclosure.querySelectorAll(".enhance-sheet-item").length + " included"
      );
    });
  }

  function enhanceDynamicContent(roots) {
    syncWorkflowProgressHeight();
    setupDatasetDropzones(roots);
    syncSpatialAlignmentScrollbars(roots);
    updateOptionalAnalysisCount(roots);
    updateExtraMaterialCount(roots);
    if (window.BuilderIcons) {
      (roots && roots.length ? roots : [document]).forEach(function (root) {
        window.BuilderIcons.decorate(root);
      });
    }
    setupRail();
    updateRailSummary();
    registerStages(roots);
    registerPrimaryAction();
    updateStatusSemantics();
    updatePipelines(roots);
    ensureDatasetSwitchVeil();
    var buildStatusHost = document.getElementById("build-stage-status");
    var clientStatus = buildStatusHost && buildStatusHost.querySelector(
      ":scope > .builder-build-status-section.is-client-build-status"
    );
    var serverStatus = document.querySelector(
      "#build_stage_status_content .builder-build-status-section"
    );
    if (clientStatus && serverStatus) clientStatus.remove();
    if (
      buildStatusScrollPhase === 2 &&
      document.querySelector(
        "#build-stage-status .builder-build-status-section:not(.is-client-build-status)"
      )
    ) {
      focusBuildStatus();
      buildStatusScrollPhase = 0;
    }
    setupFirstRun();
    if (document.querySelector(".result-card.success")) {
      var guide = document.querySelector(".builder-first-run");
      if (guide) guide.hidden = true;
      try { window.localStorage.setItem(firstRunKey, "dismissed"); } catch (error) {}
    }
    updateDialogLock();
    setupPersistentDisclosures(roots);
    setupViewerContentAccordions(roots);
    setupViewerGroupCatalogs(roots);
    setupViewerContentCatalogs(roots);
    setupCreatableSelects(roots);
    if (desiredSpatialSection) {
      var sectionSelect = document.getElementById("enhance-active_section");
      if (sectionSelect && sectionSelect.value !== desiredSpatialSection) {
        if (sectionSelect.selectize) {
          sectionSelect.selectize.setValue(desiredSpatialSection, true);
        } else {
          sectionSelect.value = desiredSpatialSection;
        }
      }
    }
    applyBuilderActivityState(roots);
    matchingDynamicElements(roots, 'input[type="color"]').forEach(enhanceColour);
    matchingDynamicElements(roots, "select[multiple]").forEach(enhanceMultiSelect);
  }

  function scheduleDynamicContentEnhancement(root) {
    dynamicContentEnhancementRoots.add(
      root && root.querySelectorAll ? root : document
    );
    if (dynamicContentEnhancementFrame !== null) return;
    dynamicContentEnhancementFrame = window.requestAnimationFrame(function () {
      dynamicContentEnhancementFrame = null;
      var roots = Array.from(dynamicContentEnhancementRoots);
      dynamicContentEnhancementRoots.clear();
      enhanceDynamicContent(roots);
    });
  }

  function handleDynamicContentMutations(mutations) {
    var isSelfManagedRailMutation = mutations.length > 0 && mutations.every(
      function (mutation) {
        var target = mutation.target.nodeType === 1
          ? mutation.target
          : mutation.target.parentElement;
        return target && target.closest(
          "#ds_ready_list, #ds_import_list, #ds_client_import_queue, " +
          ".builder-load-time"
        );
      }
    );
    if (isSelfManagedRailMutation) return;
    var ignoredSelector =
      ".selectize-control, .builder-icon-slot, .shiny-notification-panel";
    var affectedSelector =
      ".shiny-html-output, .modal, .builder-dialog, " +
      "[data-workflow-stage], #build-stage-status, #builder-operation-overlay";
    var relevantTargets = new Set();
    mutations.forEach(function (mutation) {
      var target = mutation.target.nodeType === 1
        ? mutation.target
        : mutation.target.parentElement;
      if (!target || target.closest(ignoredSelector)) return;
      var affected = target.closest(affectedSelector);
      if (affected) {
        relevantTargets.add(affected);
        return;
      }
      Array.from(mutation.addedNodes || []).forEach(function (node) {
        if (
          node.nodeType === 1 &&
          !node.closest(ignoredSelector) &&
          (node.matches(affectedSelector) || node.querySelector(affectedSelector))
        ) {
          relevantTargets.add(node);
        }
      });
    });
    if (!relevantTargets.size) return;
    var roots = Array.from(relevantTargets);
    roots.forEach(scheduleDynamicContentEnhancement);
    updateDatasetLoadTimes(roots);
    scheduleDatasetLoadTimeUpdates();
  }

  function setAttachmentEditing(row, editing) {
    var name = row.querySelector(".enhance-attachment-name");
    var editor = row.querySelector(".enhance-attachment-editor");
    var edit = row.querySelector(".enhance-attachment-edit");
    var save = row.querySelector(".enhance-attachment-save");
    var cancel = row.querySelector(".enhance-attachment-cancel");
    var input = editor && editor.querySelector("input");
    if (!name || !editor || !edit || !save || !cancel || !input) return;
    if (editing) {
      input.dataset.originalValue = input.value;
      name.hidden = true;
      editor.hidden = false;
      edit.hidden = true;
      save.hidden = false;
      cancel.hidden = false;
      input.focus();
    } else {
      name.hidden = false;
      editor.hidden = true;
      edit.hidden = false;
      save.hidden = true;
      cancel.hidden = true;
    }
  }

  function announceAddedTables(message) {
    var workbooks = message && Array.isArray(message.workbooks)
      ? message.workbooks.filter(function (workbook) {
        return workbook && workbook.key && Number(workbook.count) > 0;
      })
      : [];
    if (!workbooks.length) return;
    var cards = [];
    document.querySelectorAll(".enhance-workbook-item[data-workbook-key]").forEach(
      function (card) {
        if (!workbooks.some(function (workbook) {
          return workbook.key === card.dataset.workbookKey;
        })) return;
        card.open = false;
        card.classList.add("enhance-workbook-item--new");
        cards.push(card);
      }
    );
    var latest = cards[cards.length - 1];
    window.clearTimeout(tableUploadHighlightTimer);
    tableUploadHighlightTimer = window.setTimeout(function () {
      document.querySelectorAll(".enhance-workbook-item--new").forEach(function (card) {
        card.classList.remove("enhance-workbook-item--new");
      });
    }, 5000);
    if (!latest) return;
    var bounds = latest.getBoundingClientRect();
    if (bounds.top < 0 || bounds.bottom > window.innerHeight) {
      latest.scrollIntoView({
        block: "nearest",
        behavior: reducedMotion.matches ? "auto" : "smooth",
      });
    }
  }

  function commitAttachmentName(row) {
    var input = row.querySelector(".enhance-attachment-editor input");
    if (!input || !input.value.trim()) return;
    var isWorkbook = Boolean(input.dataset.workbookKey);
    send("enhance-table_action", {
      action: isWorkbook ? "rename_workbook" : "rename",
      key: isWorkbook ? input.dataset.workbookKey : input.dataset.tableKey,
      name: input.value.trim(),
      nonce: Date.now(),
    });
  }

  function applyAttachmentSaved(message) {
    if (!message || !message.kind || !message.key || !message.name) return;
    var selector = message.kind === "workbook"
      ? ".enhance-workbook-item[data-workbook-key]"
      : ".enhance-sheet-item";
    var row = Array.from(document.querySelectorAll(selector)).find(function (item) {
      var input = item.querySelector(".enhance-attachment-editor input");
      return input && (message.kind === "workbook"
        ? input.dataset.workbookKey === message.key
        : input.dataset.tableKey === message.key);
    });
    if (!row) return;
    var input = row.querySelector(".enhance-attachment-editor input");
    var name = row.querySelector(".enhance-attachment-name");
    input.value = message.name;
    input.dataset.originalValue = message.name;
    if (name) name.textContent = message.name;
    setAttachmentEditing(row, false);
  }

  function reopenWorkbookAfterRename(message) {
    var key = message && message.key;
    if (!key) return;
    document.querySelectorAll(".enhance-workbook-item[data-workbook-key]").forEach(
      function (workbook) {
        if (workbook.dataset.workbookKey === key) workbook.open = true;
      }
    );
  }

  document.addEventListener("click", function (event) {
    var target = event.target;
    var spatialImageTrigger = target.closest(".enhance-tissue-file-button");
    if (spatialImageTrigger) {
      var spatialImageInput = document.getElementById(
        spatialImageTrigger.getAttribute("for")
      );
      if (spatialImageInput) spatialImageInput.value = "";
    }
    if (target.closest('[aria-disabled="true"]')) {
      event.preventDefault();
      event.stopPropagation();
      return;
    }
    var reviewButton = target.closest("#continue_to_review");
    if (reviewButton && !reviewButton.disabled) {
      reviewButton.disabled = true;
      reviewButton.setAttribute("aria-busy", "true");
      reviewButton.textContent = "Opening Review…";
    }
    var continueButton = target.closest("#confirm_review");
    if (continueButton && !continueButton.disabled) {
      continueButton.disabled = true;
      continueButton.setAttribute("aria-busy", "true");
      continueButton.textContent = "Opening Build…";
    }
    if (target.closest("#build")) showImmediateBuildStatus();
    var authOpen = target.closest(".builder-auth-open");
    if (authOpen) {
      event.preventDefault();
      openAuthDialog(authOpen);
      return;
    }
    if (target.closest(".builder-auth-add")) {
      event.preventDefault();
      var rows = authRows();
      rows.push(authNewAccount());
      authRender(rows);
      var lastUsername = document.querySelector(".builder-auth-row:last-child .builder-auth-username");
      if (lastUsername) lastUsername.focus();
      return;
    }
    var authRemove = target.closest(".builder-auth-remove");
    if (authRemove) {
      event.preventDefault();
      authRemove.closest(".builder-auth-row").remove();
      return;
    }
    if (target.closest(".builder-auth-cancel")) {
      event.preventDefault();
      if (authEditor.saving) return;
      restoreAuthSnapshot();
      closeAuthDialog(true);
      return;
    }
    if (target.closest(".builder-auth-save")) {
      event.preventDefault();
      if (authEditor.saving) return;
      var accounts = authRows();
      var users = accounts.map(function (account) { return account.username.trim(); });
      var valid = accounts.length && users.every(Boolean) &&
        new Set(users).size === users.length &&
        accounts.every(function (account) { return account.password.length >= 8; });
      var authError = document.getElementById("builder-auth-error");
      if (!valid) {
        if (authError) {
          authError.textContent = "Add unique usernames and passwords of at least 8 characters.";
          authError.hidden = false;
        }
        return;
      }
      accounts.forEach(function (account, index) { account.username = users[index]; });
      authEditor.committed = authCopy(accounts);
      var nonce = Date.now();
      setAuthSaving(nonce);
      send("builder_auth_accounts", { enabled: true, accounts: accounts, nonce: nonce });
      return;
    }
    if (target.matches("#build_require_login") && !target.checked) {
      clearAuthSecrets();
      authRender([]);
      send("builder_auth_accounts", { enabled: false, accounts: [], nonce: Date.now() });
      send("builder_auth_accounts", null);
      return;
    }
    var groupColorToggle = target.closest(".group-color-toggle");
    if (groupColorToggle) {
      event.preventDefault();
      toggleGroupColors(groupColorToggle);
      return;
    }
    var viewerGroupFocus = target.closest(".viewer-group-focus");
    if (viewerGroupFocus) {
      event.preventDefault();
      focusViewerGroup(viewerGroupFocus);
      return;
    }
    var viewerGroupSelect = target.closest(".viewer-group-select");
    if (viewerGroupSelect) {
      event.preventDefault();
      selectViewerGroups(viewerGroupSelect);
      return;
    }
    var viewerMetadataSelect = target.closest(".viewer-metadata-select");
    if (viewerMetadataSelect) {
      event.preventDefault();
      selectViewerMetadata(viewerMetadataSelect);
      return;
    }
    var attachmentEdit = target.closest(".enhance-attachment-edit");
    if (attachmentEdit) {
      event.preventDefault();
      event.stopPropagation();
      setAttachmentEditing(
        attachmentEdit.closest(".enhance-sheet-item, .enhance-workbook-item"),
        true
      );
      return;
    }
    var attachmentCancel = target.closest(".enhance-attachment-cancel");
    if (attachmentCancel) {
      event.preventDefault();
      event.stopPropagation();
      var cancelRow = attachmentCancel.closest(
        ".enhance-sheet-item, .enhance-workbook-item"
      );
      var cancelInput = cancelRow.querySelector(".enhance-attachment-editor input");
      if (cancelInput) cancelInput.value = cancelInput.dataset.originalValue || cancelInput.value;
      setAttachmentEditing(cancelRow, false);
      return;
    }
    var attachmentSave = target.closest(".enhance-attachment-save");
    if (attachmentSave) {
      event.preventDefault();
      event.stopPropagation();
      commitAttachmentName(attachmentSave.closest(
        ".enhance-sheet-item, .enhance-workbook-item"
      ));
      return;
    }
    var removeWorkbook = target.closest(".enhance-workbook-remove");
    if (removeWorkbook) {
      event.preventDefault();
      event.stopPropagation();
      send("enhance-table_action", {
        action: "remove_workbook",
        key: removeWorkbook.dataset.workbookKey,
        nonce: Date.now(),
      });
      return;
    }
    var removeTable = target.closest(".enhance-table-remove");
    if (removeTable) {
      event.preventDefault();
      send("enhance-table_action", {
        action: "remove",
        key: removeTable.dataset.tableKey,
        nonce: Date.now(),
      });
      return;
    }
    var confirmMarkerSource = target.closest(".marker-source-confirm");
    if (confirmMarkerSource) {
      event.preventDefault();
      send("enhance-marker_source_confirm", {
        id: confirmMarkerSource.dataset.sourceId,
        nonce: Date.now(),
      });
      return;
    }
    var infoButton = target.closest(".enhance-info-button");
    if (infoButton) {
      event.preventDefault();
      event.stopPropagation();
      showAnalysisInfo(infoButton);
      return;
    }
    var dismissGuide = target.closest(".builder-first-run-dismiss");
    if (dismissGuide) {
      dismissGuide.closest(".builder-first-run").hidden = true;
      try { window.localStorage.setItem(firstRunKey, "dismissed"); } catch (error) {}
      return;
    }
    var managerSummary = target.closest(".rail-summary");
    if (managerSummary) {
      openDatasetManager();
      return;
    }
    if (
      target.closest(".rail-manager-close") ||
      target.closest(".rail-manager-backdrop")
    ) {
      closeDatasetManager();
      return;
    }

    var removeDataset = target.closest(".builder-drop");
    if (removeDataset) {
      event.preventDefault();
      event.stopPropagation();
      if (removeDataset.dataset.confirm === "true") {
        showRemoveConfirmation(removeDataset);
      } else {
        send("drop_ds", { id: removeDataset.dataset.ds, confirmed: true });
      }
      return;
    }

    var pickImport = target.closest(".builder-pick-import");
    if (pickImport) {
      send("pick_import", {
        id: pickImport.dataset.importId,
        nonce: Date.now(),
      });
      if (narrowManager.matches) closeDatasetManager();
      return;
    }

    var retryImport = target.closest(".builder-retry-import");
    if (retryImport) {
      event.preventDefault();
      event.stopPropagation();
      if (clientImportQueue.length) return;
      send("retry_import", {
        id: retryImport.dataset.importId,
        nonce: Date.now(),
      });
      return;
    }

    var removeImport = target.closest(".builder-remove-import");
    if (removeImport) {
      event.preventDefault();
      event.stopPropagation();
      send("remove_import", {
        id: removeImport.dataset.importId,
        nonce: Date.now(),
      });
      return;
    }

    var cancelClientImportButton = target.closest(".builder-cancel-client-import");
    if (cancelClientImportButton) {
      cancelClientImport(cancelClientImportButton.dataset.clientImportId);
      return;
    }

    var removeClientFailure = target.closest(".builder-remove-client-failure");
    if (removeClientFailure) {
      removeClientImportFailure(removeClientFailure.dataset.clientImportId);
      return;
    }

    var retryClientFailure = target.closest(".builder-retry-client-import");
    if (retryClientFailure) {
      retryClientImportFailure(retryClientFailure.dataset.clientImportId);
      return;
    }

    var railBrowser = target.closest(".builder-rail-add-browser");
    if (railBrowser) {
      openDatasetPicker();
      return;
    }

    var railLocal = target.closest(".builder-rail-add-local");
    if (railLocal) {
      if (!datasetMutationsLocked && activityCapability("add_dataset")) {
        send("choose_local_datasets", Date.now());
      }
      return;
    }

    var reorderDataset = target.closest(".builder-reorder");
    if (reorderDataset) {
      send("reorder_ds", {
        id: reorderDataset.dataset.ds,
        direction: reorderDataset.dataset.direction,
      });
      return;
    }
    var pick = target.closest(".builder-pick");
    if (pick) {
      if (beginDatasetSwitch(pick.dataset.ds)) {
        send("pick", {
          id: pick.dataset.ds,
          switch_token: datasetSwitchState.generation,
        });
      }
      if (narrowManager.matches) closeDatasetManager();
      return;
    }
    var example = target.closest(".example-btn");
    if (example) {
      enqueueExample({
        exampleId: example.dataset.ex,
        name: example.dataset.label || "Selected example",
      });
    }
  });

  document.addEventListener("keydown", function (event) {
    if (event.target.closest('[aria-disabled="true"]')) {
      event.preventDefault();
      return;
    }
    if (event.target.matches(".enhance-attachment-editor input")) {
      if (event.key === "Enter") {
        event.preventDefault();
        commitAttachmentName(event.target.closest(
          ".enhance-sheet-item, .enhance-workbook-item"
        ));
      } else if (event.key === "Escape") {
        event.preventDefault();
        var editRow = event.target.closest(
          ".enhance-sheet-item, .enhance-workbook-item"
        );
        event.target.value = event.target.dataset.originalValue || event.target.value;
        setAttachmentEditing(editRow, false);
      }
      return;
    }
    var fileTrigger = event.target.closest(".builder-file-trigger");
    if (
      fileTrigger &&
      (event.key === "Enter" || event.key === " ")
    ) {
      event.preventDefault();
      var targetId = fileTrigger.getAttribute("for");
      var fileInput = targetId ? document.getElementById(targetId) : null;
      if (fileInput) fileInput.click();
      return;
    }
    var enhanceCheckbox = event.target.closest(".enhance-module-checkbox");
    if (
      enhanceCheckbox &&
      event.key === "Enter" &&
      !enhanceCheckbox.disabled
    ) {
      event.preventDefault();
      enhanceCheckbox.click();
      return;
    }
    if (!isTextInput(event.target)) {
      var modifier = event.ctrlKey || event.metaKey;
      if (modifier && pageShortcutBlocked()) return;
      if (modifier && event.key === "Enter") {
        event.preventDefault();
        var build = document.getElementById("build");
        if (build && !build.disabled) build.click();
        return;
      }
      if (modifier && event.code === "KeyO") {
        event.preventDefault();
        openDatasetPicker();
        return;
      }
      if (modifier && event.code === "KeyZ") {
        var undo = document.getElementById("undo_remove");
        if (undo) {
          event.preventDefault();
          undo.click();
        }
        return;
      }
    }
    var pick = event.target.closest(".builder-pick");
    if (!pick || !event.altKey) return;
    var direction = null;
    if (event.key === "ArrowUp") direction = "up";
    if (event.key === "ArrowDown") direction = "down";
    if (!direction) return;
    event.preventDefault();
    send("reorder_ds", { id: pick.dataset.ds, direction: direction });
  });

  document.addEventListener("input", function (event) {
    if (event.target.matches(".viewer-point-size-input")) {
      updateProjectionPointSize(event.target, false);
      return;
    }
    if (event.target.matches(".viewer-cell-percentage-input")) {
      updateProjectionCellPercentage(event.target, false);
      return;
    }
    if (event.target.matches(".viewer-group-search")) {
      filterViewerGroups(event.target);
      return;
    }
    if (event.target.matches(".group-color-search")) {
      filterGroupColors(event.target);
      return;
    }
    if (event.target.matches('input[type="color"]')) {
      enhanceColour(event.target);
    }
  });

  document.addEventListener("change", function (event) {
    if (event.target.id === "enhance-active_section") {
      desiredSpatialSection = event.target.value;
    }
    if (event.target.id.indexOf("enhance-marker_source_mode_") === 0) {
      send("enhance-marker_source_mode", {
        id: event.target.id.replace("enhance-marker_source_mode_", ""),
        mode: event.target.value,
        nonce: Date.now(),
      });
      return;
    }
    if (event.target.matches("#dataset_files")) return;
    if (event.target.matches(".enhance-module-checkbox")) {
      updateOptionalAnalysisCount();
    }
    if (event.target.matches(".viewer-group-include")) {
      updateViewerGroupSelection(
        event.target.closest(".viewer-group-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-metadata-retain")) {
      updateViewerMetadataSelection(
        event.target.closest(".viewer-group-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-group-default")) {
      var groupRow = event.target.closest(".viewer-group-row");
      var groupCheckbox = groupRow &&
        groupRow.querySelector(".viewer-group-include");
      if (groupCheckbox && !groupCheckbox.disabled) groupCheckbox.checked = true;
      updateViewerGroupSelection(
        event.target.closest(".viewer-group-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-projection-include, .viewer-projection-default")) {
      updateProjectionSelection(
        event.target.closest(".viewer-projection-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".viewer-point-size-input")) {
      updateProjectionPointSize(event.target, true);
      return;
    }
    if (event.target.matches(".viewer-cell-percentage-input")) {
      updateProjectionCellPercentage(event.target, true);
      return;
    }
    if (event.target.matches(".viewer-trajectory-include, .viewer-trajectory-default")) {
      updateTrajectorySelection(
        event.target.closest(".viewer-trajectory-workspace"),
        true
      );
      return;
    }
    if (event.target.matches(".group-color-input")) {
      updateGroupColor(event.target);
      return;
    }
  });

  function focusDatasetStart(message) {
    var dataset = message && message.dataset;
    if (typeof dataset !== "string" || !dataset) return;
    datasetStartFocusToken += 1;
    var token = datasetStartFocusToken;
    var attempts = 0;
    function apply() {
      if (token !== datasetStartFocusToken) return;
      var selected = document.querySelector(
        "#ds_ready_list .ds[data-ds] .builder-pick[aria-current=true]"
      );
      var row = selected && selected.closest(".ds[data-ds]");
      var stage = document.querySelector('[data-workflow-stage="configure"]');
      var heading = stage && stage.querySelector("h2");
      if (!row || row.dataset.ds !== dataset || !heading) {
        attempts += 1;
        if (attempts < 12) window.setTimeout(apply, 50);
        return;
      }
      window.scrollTo({
        top: 0,
        behavior: reducedMotion.matches ? "auto" : "smooth",
      });
      heading.setAttribute("tabindex", "-1");
      heading.focus({ preventScroll: true });
    }
    apply();
  }

  function animateCoordinateResetSliders(message) {
    var ids = message && Array.isArray(message.ids) ? message.ids : [];
    ids.forEach(function (id) {
      if (!coordinateResetSliderIds.has(id)) return;
      var input = document.getElementById(id);
      var container = input && input.closest(".shiny-input-container");
      if (!container) return;
      var timer = coordinateResetMotionTimers.get(id);
      if (timer) window.clearTimeout(timer);
      container.classList.remove("builder-slider-reset-motion");
      void container.offsetWidth;
      container.classList.add("builder-slider-reset-motion");
      coordinateResetMotionTimers.set(id, window.setTimeout(function () {
        container.classList.remove("builder-slider-reset-motion");
        coordinateResetMotionTimers.delete(id);
      }, 380));
    });
  }

  function registerBuildDialogHandler() {
    if (buildDialogHandlerRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler("builder_build_dialog", showBuildDialog);
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_rail_patch",
      reconcileDatasetRail
    );
    window.Shiny.addCustomMessageHandler(
      "builder_import_rail_patch",
      reconcileImportRail
    );
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_mutation_lock",
      function (message) {
        datasetMutationsLocked = Boolean(message && message.locked === true);
        applyDatasetMutationLock();
      }
    );
    window.Shiny.addCustomMessageHandler(
      "builder_activity_state",
      function (message) {
        if (!message || typeof message !== "object") return;
        builderActivityState = {
          phase: message.phase || "none",
          capabilities: message.capabilities || {},
          busy_title: message.busy_title || null,
          busy_message: message.busy_message || null,
          busy_detail: message.busy_detail || null,
          has_project: message.has_project === true,
          open_cancelable: message.open_cancelable === true,
          warn_before_unload: message.warn_before_unload === true,
          page_inert: message.page_inert === true,
        };
        applyBuilderActivityState();
      }
    );
    window.Shiny.addCustomMessageHandler(
      "builder_worker_status",
      updateBuilderWorkerStatus
    );
    window.Shiny.addCustomMessageHandler(
      "builder_project_save_result",
      showBuilderProjectSaveResult
    );
    window.Shiny.addCustomMessageHandler(
      "builder_project_source_progress",
      updateBuilderProjectSourceProgress
    );
    window.Shiny.addCustomMessageHandler(
      "builder_project_crb_progress",
      updateBuilderProjectCrbProgress
    );
    window.Shiny.addCustomMessageHandler(
      "builder_dataset_switch_state",
      updateDatasetSwitchPhase
    );
    window.Shiny.addCustomMessageHandler(
      "enhance_tables_added",
      announceAddedTables
    );
    window.Shiny.addCustomMessageHandler(
      "enhance_attachment_saved",
      applyAttachmentSaved
    );
    window.Shiny.addCustomMessageHandler(
      "enhance_workbook_reopen",
      reopenWorkbookAfterRename
    );

    window.Shiny.addCustomMessageHandler(
      "builder_dataset_check_state",
      function (message) {
        var button = document.getElementById("complete_dataset_check");
        if (!button) return;
        var active = !!(message && message.active);
        if (active) {
          if (!button.dataset.builderCheckLabel) {
            button.dataset.builderCheckLabel = button.textContent;
          }
          button.disabled = true;
          button.setAttribute("aria-busy", "true");
          button.textContent = "Finishing check…";
          return;
        }
        if (button.dataset.builderCheckLabel) {
          button.textContent = button.dataset.builderCheckLabel;
          delete button.dataset.builderCheckLabel;
          button.disabled = false;
        }
        button.removeAttribute("aria-busy");
      }
    );
    window.Shiny.addCustomMessageHandler("builder_marker_dialog", setMarkerDialog);
    window.Shiny.addCustomMessageHandler(
      "builder_focus_dataset_start",
      focusDatasetStart
    );
    window.Shiny.addCustomMessageHandler(
      "builder_coordinate_reset_motion",
      animateCoordinateResetSliders
    );
    window.Shiny.addCustomMessageHandler(
      "builder_focus_build_status",
      function (message) {
        if (buildOperationActive) {
          beginBuildOperationFocus(
            document.getElementById("builder-operation-overlay")
          );
          return;
        }
        scheduleBuildStatusFocus();
      }
    );
    window.Shiny.addCustomMessageHandler("builder_focus_stage", function (message) {
      var id = message && message.id;
      if (["upload", "configure", "review", "build"].indexOf(id) < 0) return;
      stageFocusToken += 1;
      var token = stageFocusToken;
      var attempts = 0;
      function apply() {
        if (token !== stageFocusToken) return;
        var stage = document.querySelector('[data-workflow-stage="' + id + '"]');
        var heading = stage && stage.querySelector("h2");
        if (!heading) {
          attempts += 1;
          if (attempts < 12) window.setTimeout(apply, 50);
          return;
        }
        heading.setAttribute("tabindex", "-1");
        if (id === "build") {
          window.scrollTo({
            top: 0,
            behavior: reducedMotion.matches ? "auto" : "smooth",
          });
        } else {
          var topbar = document.querySelector(".topbar");
          var workflowBar = document.querySelector("#workflow_progress");
          var stickyBottom = Math.max(
            topbar ? topbar.getBoundingClientRect().bottom : 0,
            workflowBar ? workflowBar.getBoundingClientRect().bottom : 0
          );
          heading.style.scrollMarginTop = Math.max(0, stickyBottom + 12) + "px";
          heading.scrollIntoView({
            block: "start",
            behavior: reducedMotion.matches ? "auto" : "smooth",
          });
        }
        heading.focus({ preventScroll: true });
        scheduleStatusAnnouncement("Opened " + id + " step.");
      }
      apply();
    });
    window.Shiny.addCustomMessageHandler("builder_import_status", function (message) {
      if (message && message.text) scheduleStatusAnnouncement(message.text);
    });
    function handleAuthStatus(message) {
      if (
        !message ||
        typeof authEditor.saving !== "number" ||
        message.nonce !== authEditor.saving
      ) return;
      var error = document.getElementById("builder-auth-error");
      setAuthSaving(false);
      if (message.ok) {
        if (error) { error.textContent = ""; error.hidden = true; }
        clearAuthLiveInputs();
        authRender([]);
        authEditor.snapshot = [];
        send("builder_auth_accounts", null);
        closeAuthDialog(true);
      } else if (error) {
        error.textContent = "Login accounts could not be saved.";
        error.hidden = false;
      }
    }
    window.__builderHandleAuthStatus = handleAuthStatus;
    window.Shiny.addCustomMessageHandler("builder_auth_status", handleAuthStatus);
    window.Shiny.addCustomMessageHandler("builder_auth_reset", function (message) {
      if (!message || message.reset !== true) return;
      setAuthSaving(false);
      clearAuthSecrets();
      clearAuthError();
      authRender([]);
      send("builder_auth_accounts", null);
    });
    buildDialogHandlerRegistered = true;
  }

  function updateDatasetLoadTimes(roots) {
    var now = Date.now();
    var running = false;
    matchingDynamicElements(roots, ".builder-load-time").forEach(function (node) {
      var fixed = Number(node.dataset.elapsedMs);
      var started = Number(node.dataset.startedAtMs);
      if (!Number.isFinite(fixed) && Number.isFinite(started)) running = true;
      var elapsed = Number.isFinite(fixed)
        ? fixed
        : Number.isFinite(started)
          ? Math.max(0, now - started)
          : NaN;
      if (!Number.isFinite(elapsed)) return;
      var text = (elapsed / 1000).toFixed(1) + "s";
      if (node.textContent !== text) node.textContent = text;
    });
    if (!roots && !running && datasetLoadTimeTimer !== null) {
      window.clearTimeout(datasetLoadTimeTimer);
      datasetLoadTimeTimer = null;
    }
    return running;
  }

  function scheduleDatasetLoadTimeUpdates() {
    if (datasetLoadTimeTimer !== null) return;
    if (!document.querySelector(".builder-load-time[data-started-at-ms]")) return;
    datasetLoadTimeTimer = window.setTimeout(function () {
      datasetLoadTimeTimer = null;
      if (updateDatasetLoadTimes()) scheduleDatasetLoadTimeUpdates();
    }, 100);
  }

  function registerClientImportHandlers() {
    if (clientImportHandlersRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_dispatch_ready",
      handleClientImportDispatchReady
    );
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_accepted",
      handleClientImportAccepted
    );
    window.Shiny.addCustomMessageHandler(
      "builder_client_import_release",
      handleClientImportRelease
    );
    window.Shiny.addCustomMessageHandler(
      "builder_import_sync",
      handleClientImportSync
    );
    window.Shiny.addCustomMessageHandler(
      "builder_import_scheduler_state",
      function (message) {
        serverImportGate = Boolean(message && message.active);
        if (!serverImportGate) dispatchNextClientImport();
      }
    );
    clientImportHandlersRegistered = true;
  }

  function registerViewerGroupHandler() {
    if (viewerGroupHandlerRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler(
      "builder_group_state",
      applyViewerGroupState
    );
    viewerGroupHandlerRegistered = true;
  }

  function registerViewerContentHandlers() {
    if (!window.Shiny) return;
    if (!viewerProjectionHandlerRegistered) {
      window.Shiny.addCustomMessageHandler(
        "builder_projection_state",
        applyViewerProjectionState
      );
      viewerProjectionHandlerRegistered = true;
    }
    if (!viewerTrajectoryHandlerRegistered) {
      window.Shiny.addCustomMessageHandler(
        "builder_trajectory_state",
        applyViewerTrajectoryState
      );
      viewerTrajectoryHandlerRegistered = true;
    }
    if (!spatialSectionHandlerRegistered) {
      window.Shiny.addCustomMessageHandler(
        "builder_spatial_section_state",
        function (message) {
          if (!message || !message.value) return;
          spatialSectionGeneration += 1;
          var generation = spatialSectionGeneration;
          spatialSectionTimers.forEach(window.clearTimeout);
          spatialSectionTimers = [];
          desiredSpatialSection = message.value;
          [0, 50, 200, 500, 1000, 2000, 5000].forEach(function (delay) {
            var timer = window.setTimeout(function () {
              if (generation !== spatialSectionGeneration) return;
              var select = document.getElementById("enhance-active_section");
              if (!select) return;
              if (select.selectize) {
                select.selectize.setValue(desiredSpatialSection, true);
              } else {
                select.value = desiredSpatialSection;
              }
            }, delay);
            spatialSectionTimers.push(timer);
          });
        }
      );
      spatialSectionHandlerRegistered = true;
    }
  }

  function requestClientImportSync() {
    uploadConnectionReady = true;
    importSyncPending = true;
    registerBuildDialogHandler();
    registerClientImportHandlers();
    registerViewerGroupHandler();
    registerViewerContentHandlers();
    send("builder_dataset_rail_sync", { nonce: Date.now() });
    send("builder_import_rail_sync", { nonce: Date.now() });
    send("builder_import_sync_request", { nonce: Date.now() });
    if (document.body) scheduleDynamicContentEnhancement();
  }

  document.addEventListener("shiny:connected", function () {
    builderConnectionReady = true;
    requestClientImportSync();
    reportClientImportQueueState();
    send("builder_client_connection", {
      status: "connected",
      nonce: Date.now(),
    });
    applyBuilderActivityState();
  });
  document.addEventListener("shiny:disconnected", function () {
    builderConnectionReady = false;
    uploadConnectionReady = false;
    importSyncPending = true;
    if (builderProjectCrbDialogActive && !builderProjectCrbTerminal) {
      failBuilderProjectCrbRequest(
        "The Builder connection was interrupted, so CRB preparation status could not be confirmed."
      );
    }
    clientImportQueue.forEach(function (entry) {
      entry.stateBeforePause = entry.state;
      entry.state = "paused";
    });
    renderClientImportQueue();
    scheduleStatusAnnouncement(
      "Connection lost. Waiting to restore the import state."
    );
    applyBuilderActivityState();
  });
  document.addEventListener("shiny:sessioninitialized", function () {
    requestClientImportSync();
  });
  document.addEventListener("shiny:conditional", function () {
    scheduleSpatialAlignmentScrollbars();
  });
  window.addEventListener("beforeunload", function (event) {
    if (
      builderActivityState.warn_before_unload !== true &&
      builderConnectionReady
    ) return;
    event.preventDefault();
    event.returnValue = "";
  });
  function initializeBuilder() {
    registerBuildDialogHandler();
    registerClientImportHandlers();
    registerViewerGroupHandler();
    registerViewerContentHandlers();

    new MutationObserver(handleDynamicContentMutations).observe(document.documentElement, {
      childList: true,
      subtree: true,
    });

    if (narrowManager.addEventListener) {
      narrowManager.addEventListener("change", applyRailMode);
      reducedMotion.addEventListener("change", updateMotionDuration);
    } else {
      narrowManager.addListener(applyRailMode);
      reducedMotion.addListener(updateMotionDuration);
    }
    updateMotionDuration();
    window.addEventListener("resize", scheduleSpatialAlignmentScrollbars, { passive: true });
    window.addEventListener("scroll", scheduleSpatialAlignmentScrollbars, { passive: true });
    window.addEventListener("resize", scheduleWorkflowCompactState, { passive: true });
    window.addEventListener("scroll", scheduleWorkflowCompactState, { passive: true });
    ensureLiveRegion();
    enhanceDynamicContent();
    updateDatasetLoadTimes();
    scheduleDatasetLoadTimeUpdates();
    scheduleWorkflowCompactState();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeBuilder, { once: true });
  } else {
    initializeBuilder();
  }
})();
