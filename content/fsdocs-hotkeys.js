// Keyboard navigation for the default template.
//
//   j / k   next / previous stop in the content, or next / previous link when a menu has the focus
//   h / l   move the focus between the main menu (left), the content and the page menu (right)
//
// The search script owns '/', Ctrl+K and Escape.

const mainMenu = document.getElementById("fsdocs-main-menu");
const main = document.querySelector("main");
const content = document.getElementById("content");
const pageMenu = document.getElementById("fsdocs-page-menu");
const searchDialog = document.querySelector("dialog");

if (main && content) {
    // Left to right, as laid out on the page.
    const regions = [mainMenu, main, pageMenu].filter(Boolean);

    if (!main.hasAttribute("tabindex")) {
        main.setAttribute("tabindex", "-1");
    }

    function isVisible(element) {
        return element.getClientRects().length > 0;
    }

    function focusElement(element, scroll) {
        if (!element) {
            return;
        }
        if (!element.hasAttribute("tabindex") && element.tabIndex < 0) {
            element.setAttribute("tabindex", "-1");
        }
        element.focus({ preventScroll: true });
        // Browsers only show :focus-visible on script focus when they judge the last interaction to be keyboard
        // input, which is not reliable; a class gives the target a consistent outline.
        document.querySelectorAll(".fsdocs-hotkey-focus").forEach(previous => previous.classList.remove("fsdocs-hotkey-focus"));
        element.classList.add("fsdocs-hotkey-focus");
        element.addEventListener("blur", () => element.classList.remove("fsdocs-hotkey-focus"), { once: true });
        if (scroll) {
            element.scrollIntoView(scroll);
        }
    }

    function regionOf(element) {
        return regions.find(region => region.contains(element)) ?? main;
    }

    // Where j / k stop in the content: every heading, plus the links the page marked with
    // data-fsdocs-nav (the namespace and type tables of the API reference, which have few headings of
    // their own). querySelectorAll returns them in document order. A stop inside a closed <details>
    // or a hidden tooltip does not count.
    function stops() {
        return Array.from(content.querySelectorAll("h1, h2, h3, h4, h5, h6, [data-fsdocs-nav]")).filter(isVisible);
    }

    function isHeading(element) {
        return /^H[1-6]$/.test(element?.tagName);
    }

    function links(region) {
        return Array.from(region.querySelectorAll("a[href]")).filter(isVisible);
    }

    // Distance between a stop and the place scrollIntoView puts it (the top of the content, minus its scroll margin).
    function topOf(element) {
        const scrollMargin = parseFloat(getComputedStyle(element).scrollMarginTop) || 0;
        return element.getBoundingClientRect().top - main.getBoundingClientRect().top - scrollMargin;
    }

    // The stop the reader is looking at: the last one at or above the top of the content.
    function currentStop(all) {
        return all.filter(stop => topOf(stop) <= 2).at(-1) ?? all[0];
    }

    function moveStop(delta) {
        const all = stops();
        if (all.length === 0) {
            return;
        }
        // The first press selects the stop in view; the following ones move from it.
        const focusedIndex = all.indexOf(document.activeElement);
        const target =
            focusedIndex < 0
                ? currentStop(all)
                : all[Math.min(Math.max(focusedIndex + delta, 0), all.length - 1)];
        // A heading goes to the top, so its section is below it. A table row only scrolls when it is
        // off screen, so a run of key presses walks down the table instead of paging it.
        focusElement(target, { block: isHeading(target) ? "start" : "nearest" });
        revealPageMenuEntry(target);
        updateAnchor(target);
    }

    // Put the anchor of the focused heading in the URL, as clicking its menu entry would, so the page URL
    // can be copied with the right anchor. replaceState keeps the history clean: one entry per page, not
    // one per key press. A table row has no anchor of its own and leaves the URL alone.
    function updateAnchor(stop) {
        const anchor = stop?.querySelector("a[href^='#']")?.getAttribute("href") ?? (stop?.id ? `#${stop.id}` : null);
        if (anchor && anchor !== location.hash) {
            history.replaceState(history.state, "", anchor);
        }
    }

    // Keep the page menu entry of the focused heading in view when the menu has its own scroll bar.
    // Instant rather than smooth, so a quick series of key presses does not leave the menu lagging.
    function revealPageMenuEntry(stop) {
        const index = stop?.dataset.fsdocsHeading;
        const entry = index && pageMenu?.querySelector(`[data-fsdocs-heading="${index}"]`);
        if (entry && isVisible(entry)) {
            entry.scrollIntoView({ block: "nearest", behavior: "instant" });
        }
    }

    function moveLink(region, delta) {
        const all = links(region);
        if (all.length === 0) {
            return;
        }
        const focusedIndex = all.indexOf(document.activeElement);
        const index = focusedIndex < 0 ? (delta > 0 ? 0 : all.length - 1) : focusedIndex + delta;
        focusElement(all[Math.min(Math.max(index, 0), all.length - 1)], { block: "nearest" });
    }

    function enterRegion(region) {
        if (region === main) {
            focusElement(currentStop(stops()) ?? main);
            return;
        }
        const all = links(region);
        if (all.length === 0) {
            return;
        }
        let target = region.querySelector(".nav-item.active a[href]");
        if (region === pageMenu) {
            // The entry of the heading currently in view, when the page menu has one.
            const stop = currentStop(stops());
            const anchor = stop?.querySelector("a[href^='#']")?.getAttribute("href") ?? (stop?.id ? `#${stop.id}` : null);
            target = anchor ? all.find(link => link.getAttribute("href") === anchor) : null;
        }
        focusElement(target && isVisible(target) ? target : all[0], { block: "nearest" });
    }

    function moveRegion(delta) {
        const visible = regions.filter(isVisible);
        const index = visible.indexOf(regionOf(document.activeElement));
        const target = visible[index + delta];
        if (target) {
            enterRegion(target);
        }
    }

    function isTyping(element) {
        const tag = element?.tagName;
        return tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT" || element?.isContentEditable;
    }

    window.addEventListener("keydown", ev => {
        if (ev.defaultPrevented || ev.isComposing || ev.altKey || ev.ctrlKey || ev.metaKey || ev.shiftKey) {
            return;
        }
        if (searchDialog?.open || isTyping(document.activeElement)) {
            return;
        }

        const region = regionOf(document.activeElement);
        switch (ev.key) {
            case "j":
                region === main ? moveStop(1) : moveLink(region, 1);
                break;
            case "k":
                region === main ? moveStop(-1) : moveLink(region, -1);
                break;
            case "h":
                moveRegion(-1);
                break;
            case "l":
                moveRegion(1);
                break;
            default:
                return;
        }
        ev.preventDefault();
    });
}
