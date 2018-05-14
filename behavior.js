+function() {

function nearest(sel, el) {
  for (; el; el = el.parentNode) {
    if (el.nodeType === 1 && el.matches(sel)) return el
  }
}

function select(e) {
  const a = nearest('a:link', e.target)
  if (a) return
  const line = nearest('.line', e.target)
  if (line) {
    const s = getSelection()
    if (s.isCollapsed)
    s.selectAllChildren(line)
  }
}

document.addEventListener('click', select)

}()
