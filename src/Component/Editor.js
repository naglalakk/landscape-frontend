/**
 * // module Editor.js
 * The main purpose of this module is 
 * to setup event triggers for editor 
 * handlers that don't take any arguments.
 */

const {EventTarget, defineEventAttribute} = require("event-target-shim")

// Extend EditorHandler with EventTarget capabilities
class EditorHandler extends EventTarget {}

defineEventAttribute(EditorHandler.prototype, "image");

// Create a new EditorHandler instance
const handler = new EditorHandler();
handler.onimage = e => console.log("onimage");

// Export the editorHandler instance as Effect EventTarget
exports._editorHandler = function() {
    return handler;
}

// Image handler. Triggers event on call
exports._imageHandlerImpl = function() {
    handler.dispatchEvent(new CustomEvent("image"));
}
