import {LitElement, html, css} from 'https://esm.sh/lit';
import {component, virtual} from 'https://esm.sh/haunted';
import copy from 'https://esm.sh/copy-to-clipboard@3.3.3';

function Navigation_Old({next, previous}) {
    return previous ? html`
        <div class="d-flex justify-content-between my-4">
            <a href="${previous}">Previous</a>
            ${next && html`<a href="${next}">Next</a>`}
        </div>` : html`
        <div class="text-end my-4">
            <a href="${next}">Next</a>
        </div>`;
}

class Navigation extends LitElement {
    static properties = {
        next: {type: String, reflect: true},
        previous: {type: String, reflect: true}
    }

    constructor(props) {
        super(props);
    }
    
    static styles = css`
      :host {
        display: flex;
        justify-content: space-between;
      }
      
      a {
        margin-top: var(--spacing-200);
        color: var(--fantomas-800);
        display: inline-block;
        text-decoration: none;
        background-color: var(--fantomas-200);
        padding: var(--spacing-50) var(--spacing-100);
      }
        
        a:hover {
            background-color: var(--fantomas-400);
            color: var(--fantomas-50);
        }
      
      a:only-child {
        text-align: center;
        margin-inline: auto;
      }
    `;
    
    render() {
        return html`
            ${this.previous ? html`<a href="${this.previous}">Previous</a>` : null}
            ${this.next ? html`<a href="${this.next}">Next</a>` : null}
        `;
    }
}

class CopyToClipboard extends LitElement {
    static properties = {
        text: {type: String, reflect: true}
    }

    constructor() {
        super();
    }

    static styles = css`
      :host {
        display: inline-block;
      }

      div {
        position: relative;
      }

      iconify-icon {
        cursor: pointer;

        &:hover + .tooltip {
          visibility: visible;
          opacity: 1;
        }
      }

      iconify-icon:hover + .tooltip {
        visibility: visible;
        opacity: 1;
      }

      .tooltip {
        visibility: hidden;
        opacity: 0;
        white-space: nowrap;
        background-color: rgba(0, 0, 0, .95);
        color: #FFF;
        text-align: center;
        border-radius: var(--radius);
        padding: var(--spacing-100);
        margin: 0;
        transition: all 200ms;
        position: absolute;
        z-index: 101;
        left: 50%;
        transform: translateX(-50%);
        bottom: 100%;

        &::after {
          content: " ";
          position: absolute;
          top: 100%;
          left: 50%;
          transform: translateX(-50%);
          border-width: var(--radius);
          border-style: solid;
          border-color: rgba(0, 0, 0, .95) transparent transparent transparent;
        }
      }
    `;

    render() {
        const copyText = ev => {
            ev.preventDefault();
            copy(this.text);

            const target = ev.target;
            target.setAttribute("icon", "ri:check-line");

            const originalText = this.text;
            this.text = "Copied!";
            setTimeout(() => {
                target.setAttribute("icon", "ph:copy-thin");
                this.text = originalText;
            }, 400);
        }

        return html`
            <div>
                <iconify-icon icon="ph:copy-thin" width="24" height="24" @click="${copyText}"></iconify-icon>
                ${this.text ? html`
                    <div class="tooltip">${this.text === "Copied!" ? "Copied!" : `Copy '${this.text}'`}</div>` : null}
            </div>
        `;
    }
}

class FantomasSetting extends LitElement {
    static properties = {
        green: {type: Boolean, reflect: true},
        orange: {type: Boolean, reflect: true},
        red: {type: Boolean, reflect: true},
        gr: {type: Boolean, reflect: true}
    }

    static styles = css`
        :host {
            display: inline-block;
        }

        :host([green]) iconify-icon {
            color: #92DC84;
        }

        :host([green]) .tooltip {
            background-color: #92DC84;
        }

        :host([green]) .tooltip::after {
            border-color: #92DC84 transparent transparent transparent;
        }

        :host([orange]) iconify-icon {
            color: #F5BF4F;
        }

        :host([orange]) .tooltip {
            background-color: #F5BF4F;
        }

        :host([orange]) .tooltip::after {
            border-color: #F5BF4F transparent transparent transparent;
        }

        :host([red]) iconify-icon {
            color: #EA7268;
        }

        :host([red]) .tooltip {
            background-color: #EA7268;
        }

        :host([red]) .tooltip::after {
            border-color: #EA7268 transparent transparent transparent;
        }

        :host([gr]) iconify-icon {
            color: #00A8E2;
        }

        :host([gr]) .tooltip {
            background-color: #00A8E2;
        }

        :host([gr]) .tooltip::after {
            border-color: #00A8E2 transparent transparent transparent;
        }

        div {
            height: var(--configuration-icon-size);
            position: relative;
        }

        img {
            box-sizing: border-box;
            padding: 4px;
            background-color: #00A8E2;
            height: var(--configuration-icon-size);
            width: var(--configuration-icon-size);
            border-radius: 12px;
            display: inline-block;
        }

        img, iconify-icon {
            position: relative;
            cursor: pointer;

            &:hover + .tooltip {
                visibility: visible;
                opacity: 1;
            }
        }

        .tooltip {
            visibility: hidden;
            opacity: 0;
            white-space: nowrap;
            font-size: 14px;
            line-height: 1.5;
            background-color: rgba(0, 0, 0, .95);
            color: #FFF;
            text-align: center;
            border-radius: var(--radius);
            padding: var(--spacing-100);
            margin: 0;
            transition: all 200ms;
            position: absolute;
            z-index: 101;
            left: 50%;
            transform: translateX(-50%);
            bottom: 100%;

            &::after {
                content: " ";
                position: absolute;
                top: 100%;
                left: 50%;
                transform: translateX(-50%);
                border-width: var(--radius);
                border-style: solid;
                border-color: rgba(0, 0, 0, .95) transparent transparent transparent;
            }
        }
    `;

    constructor(props) {
        super(props);
        this.green = false;
        this.orange = false;
        this.red = false;
        this.gr = false;
    }

    render() {
        const root = document.documentElement.dataset.root
        let icon;
        let iconTooltip = "If you use one of these you should use all G-Research settings for consistency reasons";
        if (this.green && !this.gr) {
            icon = "lets-icons:check-fill";
            iconTooltip = "It is ok to change the value of this setting.";
        } else if (this.orange) {
            icon = 'material-symbols:warning';
            iconTooltip = "Changing the default of this setting is not recommended.";
        } else if (this.red) {
            icon = 'solar:danger-circle-bold-duotone'
            iconTooltip = "You shouldn't use this setting.";
        } else {
            icon = "ph:question-duotone";
        }

        return html`
            <div>
                ${!this.gr ? html`
                    <iconify-icon icon="${icon}" width="24" height="24"></iconify-icon>` : null}
                ${this.gr ? html`<img src="${root}/images/gresearch.svg" alt="G-Research logo"/>` : null}
                <div class="tooltip">${iconTooltip}</div>
            </div>`
    }
}

customElements.define('fantomas-setting', FantomasSetting);
customElements.define('copy-to-clipboard', CopyToClipboard);
customElements.define('fantomas-nav', Navigation);