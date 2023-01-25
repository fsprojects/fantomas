import {html} from 'https://cdn.skypack.dev/lit';
import {component} from 'https://cdn.skypack.dev/haunted';

function FantomasSettingIcon({type}) {
    let settingType
    switch (type) {
        case 'green':
            settingType = {
                icon: "bi-check-circle-fill",
                color: "green-recommendation",
                tooltip: "It is ok to change the value of this setting."
            }
            break;
        case 'orange':
            settingType = {
                icon: "bi-exclamation-circle-fill",
                color: "orange-recommendation",
                tooltip: "Changing the default of this setting is not recommended."
            }
            break;
        case 'red':
            settingType = {
                icon: "bi-x-circle-fill", 
                color: "red-recommendation", 
                tooltip: "You shouldn't use this setting."
            }
            break;
        case 'gr':
            const root = document.documentElement.dataset.root
            const tooltip = "If you use one of these you should use all G-Research settings for consistency reasons";

            return html`<img class="gresearch-recommendation me-2"
                             data-bs-toggle="tooltip"
                             data-bs-custom-class="gresearch-tooltip"
                             data-bs-title="${tooltip}"
                             src="${root}/images/gresearch.svg" alt="G-Research logo"/>`;
        default:
            throw `The "type" can only be "green", "orange", "red" or "gr". Found "${type}"`;
    }
    return html`<i class="bi ${settingType.icon} ${settingType.color} me-2"
                   data-bs-toggle="tooltip" data-bs-custom-class="${type}-tooltip"
                   data-bs-title="${settingType.tooltip}"></i>`;
}

function Navigation({next, previous}) {
    return previous ? html`
        <div class="d-flex justify-content-between my-4">
            <a href="${previous}">Previous</a>
            ${next && html`<a href="${next}">Next</a>`}
        </div>` : html`
        <div class="text-end my-4">
            <a href="${next}">Next</a>
        </div>`;
}

function FantomasSetting({name, green, orange, red, gr}) {
    return html`
        <div class="d-flex align-items-center my-2">
            ${green && FantomasSettingIcon({type: "green"})}
            ${orange && FantomasSettingIcon({type: "orange"})}
            ${red && FantomasSettingIcon({type: "red"})}
            ${gr && FantomasSettingIcon({type: "gr"})}
            <h4 id="${name}" class="m-0">
                <a href="#${name}">${name}</a>
            </h4>
        </div>`
}

customElements.define('fantomas-setting-icon', component(FantomasSettingIcon, {
    useShadowDOM: false, observedAttributes: ['type']
}));
customElements.define('fantomas-setting', component(FantomasSetting, {
    useShadowDOM: false, observedAttributes: ['name', 'green', 'orange', 'red', 'gr']
}));

customElements.define('fantomas-nav', component(Navigation, {
    useShadowDOM: false, observedAttributes: ['next', 'previous']
}))