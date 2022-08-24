import {html, css, LitElement} from 'https://cdn.skypack.dev/lit';
export class GreenCategory extends LitElement {
  
    static properties = {
      tooltip: {type: String},
    };

    createRenderRoot() {
      return this;
    } 
    constructor() {
      super();
      this.tooltip = 'This setting is good to use';
    }
  
    render() {
      return html`<i class="bi bi-check-circle-fill green-recommendation" data-bs-toggle="tooltip" data-bs-custom-class="green-tooltip" data-bs-title="${this.tooltip}"></i>`;
    }
  }
export class OrangeCategory extends LitElement {
  
    static properties = {
      tooltip: {type: String},
    };

    createRenderRoot() {
      return this;
    } 
    constructor() {
      super();
      this.tooltip = 'This setting is not recommended';
    }
  
    render() {
      return html`<i class="bi bi-exclamation-circle-fill orange-recommendation" data-bs-toggle="tooltip" data-bs-custom-class="orange-tooltip" data-bs-title="${this.tooltip}"></i>`;
    }
  }
  export class RedCategory extends LitElement {
  
    static properties = {
      tooltip: {type: String},
    };

    createRenderRoot() {
      return this;
    } 
    constructor() {
      super();
      this.tooltip = `You shouldn't use this setting`;
    }
  
    render() {
      return html`<i class="bi bi-x-circle-fill red-recommendation" data-bs-toggle="tooltip" data-bs-custom-class="red-tooltip" data-bs-title="${this.tooltip}"></i>`;
    }
  }
  export class GResearchCategory extends LitElement {
  
    static properties = {
      tooltip: {type: String},
      root: {type: String}
    };

    createRenderRoot() {
      return this;
    } 
    constructor() {
      super();
      this.tooltip = `If you use one of these you should use all G-Research settings for consistency reasons`;
    }
  
    render() {
      return html`<img class="gresearch-recommendation" data-bs-toggle="tooltip" data-bs-custom-class="gresearch-tooltip" data-bs-title="${this.tooltip}" data-bs-custom-class="gresearch-tooltip" src="${this.root}gresearch.svg" alt="G-Research logo"/>`;
    }
  }
  customElements.define('fantomas-green', GreenCategory);
  customElements.define('fantomas-orange', OrangeCategory);
  customElements.define('fantomas-red', RedCategory);
  customElements.define('fantomas-gresearch', GResearchCategory);