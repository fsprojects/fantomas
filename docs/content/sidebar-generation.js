import {html, render} from 'https://cdn.skypack.dev/lit-html';

window.addEventListener("load", () => {
    const sidebar = document.getElementById("sidebar-generation");
    const navHeader = document.querySelector("#sidebar-generation .nav-header");
    const navItems = [...document.querySelectorAll("#sidebar-generation .nav-item")].map(navItem => {
        const link = navItem.querySelector("a").getAttribute("href")
        const content = navItem.querySelector("a").textContent.trim();
        return html`
            <li><a href="${link}" class="ms-4 my-2 d-block">${content}</a></li>`;
    });
    const rootItem = (name, items) => {
        const id = `menu-${name.trim().replace(' ', '-').toLowerCase()}-collapse`;
        return html`
            <li class="mb-1">
                <button class="btn align-items-center rounded" data-bs-toggle="collapse"
                        data-bs-target="#${id}" aria-expanded="true">
                    ${name}
                </button>
                <div class="collapse show" id="${id}">
                    <ul class="list-unstyled fw-normal pb-1 small">
                        ${items}
                    </ul>
                </div>
            </li>`
    };
    sidebar.innerHTML = "";
    render(rootItem(navHeader.textContent, navItems), sidebar);
});