# CoffeeScript 2 code – runs in browser after compilation

specUrl = "./openapi.yaml"

# Clear previous content
clearViewer = ->
  viewer = document.getElementById 'viewer'
  viewer.innerHTML = ''
  viewer.removeAttribute 'style'
  viewer.className = 'viewer-reset'

# Render functions
renderSwagger = ->
  clearViewer()
  SwaggerUIBundle
    url: specUrl
    dom_id: '#viewer'
    deepLinking: true
    presets: [SwaggerUIBundle.presets.apis]
    docExpansion: "none"

renderRedoc = ->
  clearViewer()
  document.getElementById('viewer').innerHTML = "<redoc spec-url='#{specUrl}'></redoc>"

renderScalar = ->
  clearViewer()
  document.getElementById('viewer').innerHTML = '<div id="scalar-app"></div>'
  Scalar.createApiReference '#scalar-app',
    url: specUrl
    configuration:
      theme: 'light'
      layout: 'modern'

renderRapiDoc = ->
  clearViewer()
  document.getElementById('viewer').innerHTML = """
    <rapi-doc spec-url="#{specUrl}" persist-api-key="true" allow-try="true" />
  """

# Map selector value → render function
renderers =
  swagger: renderSwagger
  redoc:   renderRedoc
  scalar:  renderScalar
  rapidoc: renderRapiDoc

onRenderChange = ->
  selected = document.getElementById('renderer-select').value
  renderers[selected]?()

document.getElementById('renderer-select').onchange = onRenderChange

onRenderChange()
