addEventListener('load', (event) => {

    // Activate icon recommendation tooltips
    const tooltipTriggerList = document.querySelectorAll('[data-bs-toggle="tooltip"]');
    const tooltipList = [...tooltipTriggerList].map(tooltipTriggerEl => new bootstrap.Tooltip(tooltipTriggerEl));

    // Handling of sidebar open/close
    addEventListener('hide.bs.collapse', (e) => {
        if (e.target && e.target.id === 'sidebar-wrapper') {
            document.getElementById('main-container').classList.add('toggled');
        } else {
            document.getElementById(e.target.id + '-wrapper').classList.remove('menu-open');
        }
    })
    addEventListener('show.bs.collapse', (e) => {
        if (e.target && e.target.id === 'sidebar-wrapper') {
            document.getElementById('main-container').classList.remove('toggled');
        } else {
            document.getElementById(e.target.id + '-wrapper').classList.add('menu-open');
        }
    })

    // Opening/collapsing menu items of sidebar depending on the current page section
    const currentPathName = window.location.pathname;
    const currentSectionFull = currentPathName.substring(0, currentPathName.lastIndexOf('/'));
    const currenSection = currentSectionFull.substring(currentSectionFull.lastIndexOf('/') + 1);

    const openMenu = (fsdocsSection) => {
        document.getElementById(fsdocsSection + '-wrapper').classList.add('menu-open');
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
// Detect if mobile so we can hide the sidebar
const boostrapSmallSize = window.matchMedia("(max-width: 576px)");
const isMobile = boostrapSmallSize.matches;
if (isMobile) {
    new bootstrap.Collapse('#sidebar-wrapper', {
        hide: true
      })
}