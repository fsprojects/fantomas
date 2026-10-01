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
        previous: {type: String, reflect: true},
        // The page's path below the docs folder, from {{fsdocs-source-filename}}, to link to its edit page.
        // Not {{fsdocs-page-source}}: `fsdocs watch` makes that one an absolute path on this machine.
        source: {type: String, reflect: true}
    }

    constructor(props) {
        super(props);
    }

    static styles = css`
      :host {
        display: block;
      }

      .contribute {
        display: flex;
        align-items: center;
        gap: var(--spacing-200);
        margin-top: var(--spacing-600);
        padding: var(--spacing-300) var(--spacing-400);
        border-radius: var(--radius);
        background-color: light-dark(var(--fantomas-100), var(--fantomas-800));
        font-size: 0.875rem;
      }

      .contribute iconify-icon {
        flex-shrink: 0;
        color: light-dark(var(--fantomas-500), var(--fantomas-300));
      }

      .contribute a {
        color: light-dark(var(--fantomas-600), var(--fantomas-200));
      }

      .contribute a:hover {
        color: var(--link-hover);
      }

      .pager {
        display: flex;
        justify-content: space-between;
        margin-top: var(--spacing-500);
        margin-bottom: var(--spacing-500);
      }

      .pager a {
        color: var(--fantomas-800);
        display: inline-block;
        text-decoration: none;
        background-color: var(--fantomas-200);
        padding: var(--spacing-50) var(--spacing-100);
        border-radius: var(--radius);
      }

        .pager a:hover {
            background-color: var(--fantomas-400);
            color: var(--fantomas-50);
        }

      .pager a:only-child {
        text-align: center;
        margin-inline: auto;
      }
    `;

    render() {
        const editLink = `https://github.com/fsprojects/fantomas/edit/main/docs/${encodeURI(this.source ?? "")}`;
        return html`
            ${this.source ? html`
                <div class="contribute">
                    <iconify-icon icon="ph:pencil-simple" width="18" height="18"></iconify-icon>
                    <span>Found something wrong or unclear on this page?
                        <a href="${editLink}" target="_blank" rel="noopener">Edit this page on GitHub</a></span>
                </div>` : null}
            <div class="pager">
                ${this.previous ? html`<a href="${this.previous}">Previous</a>` : null}
                ${this.next ? html`<a href="${this.next}">Next</a>` : null}
            </div>
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
                ${this.gr ? html`<img src="${root}images/gresearch.svg" alt="G-Research logo"/>` : null}
                <div class="tooltip">${iconTooltip}</div>
            </div>`
    }
}

customElements.define('fantomas-setting', FantomasSetting);
customElements.define('copy-to-clipboard', CopyToClipboard);
customElements.define('fantomas-nav', Navigation);