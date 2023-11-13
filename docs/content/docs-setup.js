addEventListener('load', (event) => {

    // Activate icon recommendation tooltips
    const tooltipTriggerList = document.querySelectorAll('[data-bs-toggle="tooltip"]');
    const tooltipList = [...tooltipTriggerList].map(tooltipTriggerEl => new bootstrap.Tooltip(tooltipTriggerEl));

    // get body element
    const body = document.getElementsByTagName('body')[0];
    
    document
        .querySelectorAll("#toggle-mobile-menu, #close-mobile-menu")
        .forEach(element => {
            element.addEventListener("click", () => {
                body.classList.toggle("open-menu");
            });
        })

    // Opening/collapsing menu items of sidebar depending on the current page section
    const activeMenuSection = location.pathname.startsWith("/reference") ? document.querySelector("#api_reference-wrapper") : document.querySelector(".accordion-item.active");
    
    if (activeMenuSection) {
        const accordion = activeMenuSection.querySelector(".accordion-collapse");
        if (accordion) {
            accordion.classList.add('show');
        }
        const button = activeMenuSection.querySelector("button");
        if (button) {
            button.classList.remove('collapsed');
        }
    }
});
