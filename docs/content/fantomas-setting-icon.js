import { html } from 'https://cdn.skypack.dev/lit';
import { component } from 'https://cdn.skypack.dev/haunted';


function FantomasSettingIcon({ tooltip, type }) {
    let settingType = 
        type === 'green' ? {icon: "bi-check-circle-fill", color: "green-recommendation", tooltip: tooltip?tooltip:"This setting is good to use"}:  
        type === 'orange' ? {icon:"bi-exclamation-circle-fill", color:"orange-recommendation", tooltip: tooltip?tooltip:"This setting is not recommended"}:
        type === 'red' ? {icon: "bi-x-circle-fill", color:"red-recommendation", tooltip: tooltip?tooltip:"You shouldn't use this setting"}:
        null

    return  html`<i class="bi ${settingType.icon} ${settingType.color}" 
                    data-bs-toggle="tooltip" data-bs-custom-class="${type}-tooltip" data-bs-title="${settingType.tooltip}"></i>`;
}

function FantomasSettingIconGResearch({ tooltip }) {
    let root = window.fsdocs_search_baseurl
    let safeTooltip =
        tooltip ? tooltip : "If you use one of these you should use all G-Research settings for consistency reasons";

    return  html`<img class="gresearch-recommendation" data-bs-toggle="tooltip" data-bs-custom-class="gresearch-tooltip" data-bs-title="${safeTooltip}" data-bs-custom-class="gresearch-tooltip" src="${root}gresearch.svg" alt="G-Research logo"/>`;
}

customElements.define('fantomas-setting-icon', component(FantomasSettingIcon, { useShadowDOM: false,
                                                                               observedAttributes: [ 'tooltip','type'] }));

customElements.define('fantomas-setting-icon-gresearch', component(FantomasSettingIconGResearch, { useShadowDOM: false,
                                                                               observedAttributes: [ 'tooltip'] }));