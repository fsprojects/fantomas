import {html} from 'https://cdn.skypack.dev/lit';
import {component} from 'https://cdn.skypack.dev/haunted';


function FantomasSettingIcon({tooltip, type}) {
    let settingType
    switch (type) {
        case 'green':
            settingType = {
                icon: "bi-check-circle-fill",
                color: "green-recommendation",
                tooltip: tooltip ? tooltip : "It is ok to change the value of this setting."
            }
            break;
        case 'orange':
            settingType = {
                icon: "bi-exclamation-circle-fill",
                color: "orange-recommendation",
                tooltip: tooltip ? tooltip : "Changing the default of this setting is not recommended."
            }
            break;
        case 'red':
            settingType = {
                icon: "bi-x-circle-fill",
                color: "red-recommendation",
                tooltip: tooltip ? tooltip : "You shouldn't use this setting."
            }
            break;
        default:
            throw "The \"type\" can only be \"green\", \"orange\" or \"red\""
    }
    return html`<i class="bi ${settingType.icon} ${settingType.color} me-2"
                   data-bs-toggle="tooltip" data-bs-custom-class="${type}-tooltip"
                   data-bs-title="${settingType.tooltip}"></i>`;
}

function FantomasSettingIconGResearch({tooltip}) {
    const root = document.documentElement.dataset.root
    const safeTooltip =
        tooltip ? tooltip : "If you use one of these you should use all G-Research settings for consistency reasons";

    return html`<img class="gresearch-recommendation me-2" data-bs-toggle="tooltip" data-bs-custom-class="gresearch-tooltip"
                   data-bs-title="${safeTooltip}" data-bs-custom-class="gresearch-tooltip" src="${root}/images/gresearch.svg" alt="G-Research logo"/>`;
}

function Navigation({next, previous}) {
    return previous ?
        html`
            <div class="d-flex justify-content-between my-4">
                <a href="${previous}">Previous</a>
                ${next && html`<a href="${next}">Next</a>`}
            </div>` :
        html`
            <div class="text-end my-4">
                <a href="${next}">Next</a>
            </div>`;
}

customElements.define('fantomas-setting-icon', component(FantomasSettingIcon, {
    useShadowDOM: false,
    observedAttributes: ['tooltip', 'type']
}));

customElements.define('fantomas-setting-icon-gresearch', component(FantomasSettingIconGResearch, {
    useShadowDOM: false,
    observedAttributes: ['tooltip']
}));

customElements.define('fantomas-nav', component(Navigation, { useShadowDOM: false, observedAttributes: ['next', 'previous']}))