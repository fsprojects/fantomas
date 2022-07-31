import {html, render} from 'https://cdn.skypack.dev/lit-html';

window.addEventListener("load", () => {
    const sidebar = document.getElementById("sidebar-generation");
    const navHeader = document.querySelector("#sidebar-generation .nav-header");
    const navItems = [...document.querySelectorAll("#sidebar-generation .nav-item")].map(navItem => {
        const link = navItem.querySelector("a").getAttribute("href")
        const content = navItem.querySelector("a").textContent.trim();
        return html`
            <li><a href="${link}" class="my-2 d-block">${content}</a></li>`;
    });
    const rootItem = (name, items) => {
        const id = `menu-${name.trim().replace(' ', '-').toLowerCase()}-collapse`;
        return html`
            <div id=${id}-wrapper class="p-2 list-wrapper menu-open">
            <li class="docs-menu">
                <button class="btn text-white d-flex p-1 justify-content-between" data-bs-toggle="collapse"
                        data-bs-target="#${id}" aria-expanded="true">
                    ${name}
            <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" stroke="currentColor" stroke-width="2" fill="currentColor" class="bi bi-chevron-down mx-1" viewBox="0 0 16 16">
  <path fill-rule="evenodd" d="M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z"/>
</svg>
                </button>
                <div id="${id}" class="ms-4 collapse show">
                    <ul class="list-unstyled fw-normal pb-1">
                        ${items}
                    </ul>
                </div>
            </li>
            </div>
            `
    };
    sidebar.innerHTML = "";
    render(rootItem(navHeader.textContent, navItems), sidebar);
});