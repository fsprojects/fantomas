window.addEventListener("load", () => {
let container = document.getElementById("sidebar-generation").childNodes;
container = Array.from(container);
const NAMESPACE = `<li class="mb-1">
<button class="btn btn-toggle align-items-center rounded collapsed nav-header" data-bs-toggle="collapse" data-bs-target="#COLLAPSE" aria-expanded="true">
  NAMESPACE
</button>
<div class="collapse show" id="COLLAPSE">
  <ul class="btn-toggle-nav list-unstyled fw-normal pb-1 small" >
  </ul>
</div>
</li>`;
container = container.filter(node => node.nodeType === Node.ELEMENT_NODE);

const ITEM = `<li class="nav-"><a href="#" class="rounded nav-link">ITEM</a></li>`;
let lastHeader = null;
for (let i = 0; i < container.length; i++) {
    if (container[i].classList.contains("nav-header")) {
        let header = container[i].innerText;
        container[i].innerHTML = NAMESPACE.replace("NAMESPACE", header).replaceAll("COLLAPSE", header.toLowerCase().replace(/\s/g, ""));
        lastHeader = container[i];
}   
    else if (container[i].classList.contains("nav-item")) {
  console.log(container[i]);
        let item = container[i].innerText;
        let href = searchTree(container[i], 'a').href;
        let ul = searchTree(lastHeader, "ul");
        ul.innerHTML += ITEM.replace("ITEM", item).replace("#", href);
    }
}
const old = document.querySelectorAll('.nav-item');

old.forEach(old => {
  old.remove();
});

});

function searchTree(element, search){
  if(element.localName == search){
       return element;
  }else if (element.childNodes != null && element.childNodes.length != 0){
       var i;
       var result = null;
       for(i=0; result == null && i < element.childNodes.length; i++){
            result = searchTree(element.childNodes[i], search);
       }
       return result;
  }
  return null;
}