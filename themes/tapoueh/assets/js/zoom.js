/* Diagram zoom.
 *
 * Every image inside .prose opens in a near-fullscreen overlay that can be
 * zoomed with the wheel (anchored at the pointer, so the thing under the
 * cursor stays under the cursor), panned by dragging, and driven from the
 * buttons or the keyboard. Figures rendered by the image shortcode are
 * wrapped in <a href="full-size">, so we open that rather than the possibly
 * smaller thumbnail the page is showing.
 *
 * No dependencies: the diagrams are SVG, and a library to pan them would
 * outweigh them several times over.
 */
(function () {
  "use strict";

  var MIN = 1, MAX = 16, STEP = 1.3;

  var overlay, stage, img, zoomLabel, opener;
  var scale = 1, tx = 0, ty = 0;
  var dragging = false, lastX = 0, lastY = 0, moved = false;
  var pointers = new Map(), pinchFrom = 0, pinchScale = 1;

  function clamp(v, lo, hi) { return v < lo ? lo : v > hi ? hi : v; }

  function apply() {
    img.style.transform =
      "translate(" + tx + "px," + ty + "px) scale(" + scale + ")";
    if (zoomLabel) zoomLabel.textContent = Math.round(scale * 100) + "%";
    overlay.classList.toggle("is-zoomed", scale > 1.01);
  }

  function reset() { scale = 1; tx = 0; ty = 0; apply(); }

  /* Zoom about a viewport point, keeping whatever sits under it in place. */
  function zoomAt(next, px, py) {
    next = clamp(next, MIN, MAX);
    if (next === scale) return;
    var r = stage.getBoundingClientRect();
    var cx = r.left + r.width / 2, cy = r.top + r.height / 2;
    if (px === undefined) { px = cx; py = cy; }
    var k = next / scale;
    tx = px - cx - (px - cx - tx) * k;
    ty = py - cy - (py - cy - ty) * k;
    scale = next;
    if (scale <= MIN + 0.001) { tx = 0; ty = 0; }
    apply();
  }

  function build() {
    overlay = document.createElement("div");
    overlay.className = "zoom-overlay";
    overlay.setAttribute("role", "dialog");
    overlay.setAttribute("aria-modal", "true");
    overlay.setAttribute("aria-label", "Diagram viewer");
    overlay.innerHTML =
      '<div class="zoom-stage"><img class="zoom-img" alt=""></div>' +
      '<div class="zoom-bar">' +
        '<button type="button" data-act="out" aria-label="Zoom out">&minus;</button>' +
        '<span class="zoom-level" aria-live="polite">100%</span>' +
        '<button type="button" data-act="in" aria-label="Zoom in">+</button>' +
        '<button type="button" data-act="reset" aria-label="Reset zoom">Reset</button>' +
        '<button type="button" data-act="close" aria-label="Close">Close</button>' +
      "</div>" +
      '<p class="zoom-hint">scroll to zoom &middot; drag to move &middot; Esc to close</p>';
    document.body.appendChild(overlay);

    stage = overlay.querySelector(".zoom-stage");
    img = overlay.querySelector(".zoom-img");
    zoomLabel = overlay.querySelector(".zoom-level");

    overlay.addEventListener("click", function (e) {
      var act = e.target.getAttribute && e.target.getAttribute("data-act");
      if (act === "in") zoomAt(scale * STEP);
      else if (act === "out") zoomAt(scale / STEP);
      else if (act === "reset") reset();
      else if (act === "close") close();
      /* a click on the backdrop closes; a drag that ended there does not */
      else if (e.target === overlay || e.target === stage) { if (!moved) close(); }
      moved = false;
    });

    stage.addEventListener("wheel", function (e) {
      e.preventDefault();
      var f = Math.pow(STEP, -e.deltaY > 0 ? 1 : -1);
      zoomAt(scale * f, e.clientX, e.clientY);
    }, { passive: false });

    img.addEventListener("dblclick", function (e) {
      if (scale > 1.01) reset(); else zoomAt(2.5, e.clientX, e.clientY);
    });

    /* Pointer events cover mouse, pen and touch, including two-finger pinch. */
    stage.addEventListener("pointerdown", function (e) {
      pointers.set(e.pointerId, e);
      if (pointers.size === 2) {
        var p = Array.from(pointers.values());
        pinchFrom = Math.hypot(p[0].clientX - p[1].clientX, p[0].clientY - p[1].clientY);
        pinchScale = scale;
        dragging = false;
        return;
      }
      dragging = true; moved = false;
      lastX = e.clientX; lastY = e.clientY;
      stage.setPointerCapture(e.pointerId);
    });

    stage.addEventListener("pointermove", function (e) {
      if (!pointers.has(e.pointerId)) return;
      pointers.set(e.pointerId, e);

      if (pointers.size === 2 && pinchFrom) {
        var p = Array.from(pointers.values());
        var d = Math.hypot(p[0].clientX - p[1].clientX, p[0].clientY - p[1].clientY);
        zoomAt(pinchScale * (d / pinchFrom),
               (p[0].clientX + p[1].clientX) / 2,
               (p[0].clientY + p[1].clientY) / 2);
        moved = true;
        return;
      }
      if (!dragging) return;
      var dx = e.clientX - lastX, dy = e.clientY - lastY;
      if (Math.abs(dx) + Math.abs(dy) > 2) moved = true;
      tx += dx; ty += dy;
      lastX = e.clientX; lastY = e.clientY;
      apply();
    });

    function release(e) {
      pointers.delete(e.pointerId);
      if (pointers.size < 2) pinchFrom = 0;
      if (pointers.size === 0) dragging = false;
    }
    stage.addEventListener("pointerup", release);
    stage.addEventListener("pointercancel", release);

    document.addEventListener("keydown", function (e) {
      if (!overlay.classList.contains("is-open")) return;
      var k = e.key, pan = e.shiftKey ? 200 : 60;
      if (k === "Escape") { close(); }
      else if (k === "+" || k === "=") { zoomAt(scale * STEP); }
      else if (k === "-" || k === "_") { zoomAt(scale / STEP); }
      else if (k === "0") { reset(); }
      else if (k === "ArrowLeft")  { tx += pan; apply(); }
      else if (k === "ArrowRight") { tx -= pan; apply(); }
      else if (k === "ArrowUp")    { ty += pan; apply(); }
      else if (k === "ArrowDown")  { ty -= pan; apply(); }
      else return;
      e.preventDefault();
    });
  }

  function open(src, alt, from) {
    if (!overlay) build();
    opener = from || null;
    img.src = src;
    img.alt = alt || "";
    reset();
    overlay.classList.add("is-open");
    document.documentElement.classList.add("zoom-lock");
    var btn = overlay.querySelector('[data-act="close"]');
    if (btn) btn.focus({ preventScroll: true });
  }

  function close() {
    if (!overlay) return;
    overlay.classList.remove("is-open");
    document.documentElement.classList.remove("zoom-lock");
    img.removeAttribute("src");
    if (opener && opener.focus) opener.focus({ preventScroll: true });
    opener = null;
  }

  function init() {
    var imgs = document.querySelectorAll(".prose img, .figure img");
    Array.prototype.forEach.call(imgs, function (el) {
      var link = el.closest("a");
      /* Only hijack a link that points at the image itself; a figure used
         as a link to somewhere else keeps working as a link. */
      var full = null;
      if (link) {
        var href = link.getAttribute("href") || "";
        if (/\.(svg|png|jpe?g|gif|webp|avif)($|\?)/i.test(href)) full = link.href;
      }
      var target = full ? link : el;
      target.classList.add("zoomable");
      if (!full) { el.setAttribute("tabindex", "0"); el.setAttribute("role", "button"); }
      target.addEventListener("click", function (e) {
        e.preventDefault();
        open(full || el.currentSrc || el.src, el.alt, target);
      });
      if (!full) {
        el.addEventListener("keydown", function (e) {
          if (e.key === "Enter" || e.key === " ") {
            e.preventDefault();
            open(el.currentSrc || el.src, el.alt, el);
          }
        });
      }
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
