addEventListener('load', (event) => {

    // Activate icon recommendation tooltips
    const tooltipTriggerList = document.querySelectorAll('[data-bs-toggle="tooltip"]');
    const tooltipList = [...tooltipTriggerList].map(tooltipTriggerEl => new bootstrap.Tooltip(tooltipTriggerEl));

    // Handling of sidebar open/close
    const sidebarWrapper = document.getElementById("sidebar-wrapper");
    const mainContainer = document.getElementById("main-container");
    document.querySelector("#toggle-mobile-menu").addEventListener("click", () => {
        console.log("clicked")
        mainContainer.classList.remove('toggled');
        sidebarWrapper.classList.toggle("d-none");
    })
    
    document.querySelector("#close-mobile-menu").addEventListener("click", () => {
        mainContainer.classList.add('toggled');
        sidebarWrapper.classList.toggle("d-none");
    });

    // Opening/collapsing menu items of sidebar depending on the current page section
    const currentPathName = window.location.pathname;
    const currentSectionFull = currentPathName.substring(0, currentPathName.lastIndexOf('/'));
    const currenSection = currentSectionFull.substring(currentSectionFull.lastIndexOf('/') + 1);

    const openMenu = (fsdocsSection) => {
        document.getElementById(fsdocsSection).classList.add('show');
        document.getElementById(fsdocsSection + '-button').classList.remove('collapsed');
    }
    switch (currenSection) {
        case 'docs':
            openMenu('end-users');
            break;
        case 'reference':
            openMenu('api_reference');
            break;
        default:
            openMenu(currenSection);
            break;
    }
});
