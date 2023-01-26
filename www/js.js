function showTooltip(evt, text) {
  let tooltip = document.getElementById('tooltip')
  tooltip.innerHTML = text
  tooltip.style.display = 'block'
  tooltip.style.left = evt.pageX + 2 + 'px'
  tooltip.style.top = evt.pageY + 2 + 'px'
}

function hideTooltip() {
  var tooltip = document.getElementById('tooltip')
  tooltip.style.display = 'none'
}

Shiny.addCustomMessageHandler(
  'resetFileInputHandlerHide',
  function (elementId) {
    //alert('testing');
    var id = elementId + '_progress'
    var elem = document.getElementById(id)
    elem.style.visibility = 'hidden'
    elem.firstChild.nextElementSibling.style.width = '0%'
  }
)

Shiny.addCustomMessageHandler(
  'resetFileInputHandlerShow',
  function (elementId) {
    //alert('testing');
    var id = elementId + '_progress'
    var elem = document.getElementById(id)
    elem.style.visibility = 'visible'
    elem.firstChild.nextElementSibling.style.width = '100%'
  }
)

function toggle(elementId) {
  var ele = document.getElementById(elementId)
  if (ele.style.display == 'block') {
    ele.style.display = 'none'
  } else {
    ele.style.display = 'block'
  }
}
