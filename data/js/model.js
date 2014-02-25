function Model() {
    this.listeners = [];
}

Model.prototype.addChangeListener = function(listener) {
    // Check for duplicates.
    for (var i = 0; i < this.listeners.length; i++) {
        if (listener == this.listeners[i]) return;
    }

    this.listeners.push(listener);
};

Model.prototype.removeChangeListener = function(listener) {
    var listeners = [];
    for (var i = 0; i < this.listeners.length; i++) {
        if (listener != this.listeners[i]) {
            listeners.push(this.listeners[i]);
        }
    }
    this.listeners = listeners;
};

Model.prototype.triggerChange = function() {
    for (var i in this.listeners) {
        this.listeners[i].onChange(this);
    }
};
