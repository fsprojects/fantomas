import {LitElement, html, css} from 'https://cdn.jsdelivr.net/gh/lit/dist@3/core/lit-core.min.js';

const prefersDark = window.matchMedia("@media (prefers-color-scheme: dark)").matches;
let currentTheme = localStorage.getItem('theme') ?? (prefersDark ? 'dark' : 'light');
if (currentTheme === 'dark') {
    window.document.documentElement.setAttribute("data-theme", 'dark');
}

export class ThemeToggle extends LitElement {
    static properties = {
        _theme: {state: true, type: String},
    };

    constructor() {
        super();
        this._theme = currentTheme;
    }

    static styles = css`
      div {
        height: 30px;
        width: 30px;
        cursor: pointer;
      }

      .light {
        background: url('https://api.iconify.design/basil/moon-solid.svg') no-repeat center center / contain;
      }

      .dark {
        background: url('https://api.iconify.design/basil/sun-solid.svg?color=white') no-repeat center center / contain;
      }
    `;

    changeTheme() {
        this._theme = this._theme === 'light' ? 'dark' : 'light';
        localStorage.setItem('theme', this._theme);
        window.document.documentElement.setAttribute("data-theme", this._theme);
    }

    render() {
        return html`
            <div class="${this._theme}" @click=${this.changeTheme}></div>
        `;
    }
}

customElements.define('fsdocs-theme-toggle', ThemeToggle);
