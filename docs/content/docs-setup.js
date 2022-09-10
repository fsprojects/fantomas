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
