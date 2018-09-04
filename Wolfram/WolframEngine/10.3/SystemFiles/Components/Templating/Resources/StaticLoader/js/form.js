/*
 * All files inside this folder will be concatenated into a single file.
 * The order is important, so make sure the file name is ordered.
 */

/**
 * register plugins
 */

!function($) {

    $.fn.registerPlugin = function(plugin, name) {
        $.fn[name] = function(option, parameter) {
            return this.each(function() {
                var data = $(this).data(name);
                var options = typeof option === 'object' && option;
                // Initialize the plugin.
                if (!data) {
                    data = new plugin(this, options);
                    $(this).data(name, data);
                }
                // Call plugin method.
                if (typeof option === 'string') {
                    data[option](parameter);
                    if (option === 'destroy') {
                        $(this).data(name, false);
                    }
                }
            });
        };
        $.fn[name].plugin = plugin;
    }

}(window.jQuery);


// Color object
var Color = function(val) {
    this.value = {
        h: 0,
        s: 0,
        v: 0,
        a: 1,
        valid: false
    };
    this.format = null; // original string format
    this.input  = val;
    if (val) {
        if (val.toLowerCase !== undefined) {
            this.setColor(val);
        } else if (val.h !== undefined) {
            this.value = val;
        }
    }
};

Color.prototype = {
    constructor: Color,
    // 140 predefined colors from the HTML Colors spec
    colors: {
        "aliceblue": "#f0f8ff",
        "antiquewhite": "#faebd7",
        "aqua": "#00ffff",
        "aquamarine": "#7fffd4",
        "azure": "#f0ffff",
        "beige": "#f5f5dc",
        "bisque": "#ffe4c4",
        "black": "#000000",
        "blanchedalmond": "#ffebcd",
        "blue": "#0000ff",
        "blueviolet": "#8a2be2",
        "brown": "#a52a2a",
        "burlywood": "#deb887",
        "cadetblue": "#5f9ea0",
        "chartreuse": "#7fff00",
        "chocolate": "#d2691e",
        "coral": "#ff7f50",
        "cornflowerblue": "#6495ed",
        "cornsilk": "#fff8dc",
        "crimson": "#dc143c",
        "cyan": "#00ffff",
        "darkblue": "#00008b",
        "darkcyan": "#008b8b",
        "darkgoldenrod": "#b8860b",
        "darkgray": "#a9a9a9",
        "darkgreen": "#006400",
        "darkkhaki": "#bdb76b",
        "darkmagenta": "#8b008b",
        "darkolivegreen": "#556b2f",
        "darkorange": "#ff8c00",
        "darkorchid": "#9932cc",
        "darkred": "#8b0000",
        "darksalmon": "#e9967a",
        "darkseagreen": "#8fbc8f",
        "darkslateblue": "#483d8b",
        "darkslategray": "#2f4f4f",
        "darkturquoise": "#00ced1",
        "darkviolet": "#9400d3",
        "deeppink": "#ff1493",
        "deepskyblue": "#00bfff",
        "dimgray": "#696969",
        "dodgerblue": "#1e90ff",
        "firebrick": "#b22222",
        "floralwhite": "#fffaf0",
        "forestgreen": "#228b22",
        "fuchsia": "#ff00ff",
        "gainsboro": "#dcdcdc",
        "ghostwhite": "#f8f8ff",
        "gold": "#ffd700",
        "goldenrod": "#daa520",
        "gray": "#808080",
        "green": "#008000",
        "greenyellow": "#adff2f",
        "honeydew": "#f0fff0",
        "hotpink": "#ff69b4",
        "indianred ": "#cd5c5c",
        "indigo ": "#4b0082",
        "ivory": "#fffff0",
        "khaki": "#f0e68c",
        "lavender": "#e6e6fa",
        "lavenderblush": "#fff0f5",
        "lawngreen": "#7cfc00",
        "lemonchiffon": "#fffacd",
        "lightblue": "#add8e6",
        "lightcoral": "#f08080",
        "lightcyan": "#e0ffff",
        "lightgoldenrodyellow": "#fafad2",
        "lightgrey": "#d3d3d3",
        "lightgreen": "#90ee90",
        "lightpink": "#ffb6c1",
        "lightsalmon": "#ffa07a",
        "lightseagreen": "#20b2aa",
        "lightskyblue": "#87cefa",
        "lightslategray": "#778899",
        "lightsteelblue": "#b0c4de",
        "lightyellow": "#ffffe0",
        "lime": "#00ff00",
        "limegreen": "#32cd32",
        "linen": "#faf0e6",
        "magenta": "#ff00ff",
        "maroon": "#800000",
        "mediumaquamarine": "#66cdaa",
        "mediumblue": "#0000cd",
        "mediumorchid": "#ba55d3",
        "mediumpurple": "#9370d8",
        "mediumseagreen": "#3cb371",
        "mediumslateblue": "#7b68ee",
        "mediumspringgreen": "#00fa9a",
        "mediumturquoise": "#48d1cc",
        "mediumvioletred": "#c71585",
        "midnightblue": "#191970",
        "mintcream": "#f5fffa",
        "mistyrose": "#ffe4e1",
        "moccasin": "#ffe4b5",
        "navajowhite": "#ffdead",
        "navy": "#000080",
        "oldlace": "#fdf5e6",
        "olive": "#808000",
        "olivedrab": "#6b8e23",
        "orange": "#ffa500",
        "orangered": "#ff4500",
        "orchid": "#da70d6",
        "palegoldenrod": "#eee8aa",
        "palegreen": "#98fb98",
        "paleturquoise": "#afeeee",
        "palevioletred": "#d87093",
        "papayawhip": "#ffefd5",
        "peachpuff": "#ffdab9",
        "peru": "#cd853f",
        "pink": "#ffc0cb",
        "plum": "#dda0dd",
        "powderblue": "#b0e0e6",
        "purple": "#800080",
        "red": "#ff0000",
        "rosybrown": "#bc8f8f",
        "royalblue": "#4169e1",
        "saddlebrown": "#8b4513",
        "salmon": "#fa8072",
        "sandybrown": "#f4a460",
        "seagreen": "#2e8b57",
        "seashell": "#fff5ee",
        "sienna": "#a0522d",
        "silver": "#c0c0c0",
        "skyblue": "#87ceeb",
        "slateblue": "#6a5acd",
        "slategray": "#708090",
        "snow": "#fffafa",
        "springgreen": "#00ff7f",
        "steelblue": "#4682b4",
        "tan": "#d2b48c",
        "teal": "#008080",
        "thistle": "#d8bfd8",
        "tomato": "#ff6347",
        "turquoise": "#40e0d0",
        "violet": "#ee82ee",
        "wheat": "#f5deb3",
        "white": "#ffffff",
        "whitesmoke": "#f5f5f5",
        "yellow": "#ffff00",
        "yellowgreen": "#9acd32"
    },
    _sanitizeNumber: function(val) {
        if (typeof val === 'number') {
            return val;
        }
        if (isNaN(val) || (val === null) || (val === '') || (val === undefined)) {
            return 1;
        }
        if (val.toLowerCase !== undefined) {
            return parseFloat(val);
        }
        return 1;
    },
    copy: function() {
        return new Color($.extend(true, {}, this.value))
    },
    //parse a string to HSV
    setColor: function(strVal) {
        strVal = strVal.toLowerCase();
        this.value = this.stringToHSV(strVal) || {
            h: 0,
            s: 0,
            v: 0,
            a: 1,
            valid: false
        };
        this.input = strVal;
    },
    stringToHSV: function(strVal) {
        strVal = strVal.toLowerCase().trim();

        var that = this,
            result = false;
        $.each(this.stringParsers, function(i, parser) {
            var match = parser.re.exec(strVal),
                values = match && parser.parse.apply(that, [match]),
                format = parser.format || 'rgba';
            if (values) {
                if (format.match(/hsla?/)) {
                    result = that.RGBtoHSV.apply(that, that.HSLtoRGB.apply(that, values));
                } else {
                    result = that.RGBtoHSV.apply(that, values);
                }
                that.format = format;
                return false;
            }
            return true;
        });
        if (result) result.valid = true;
        return result;
    },
    toRGB: function(h, s, v, a) {

        if (h == undefined) h = this.value.h;
        if (s == undefined) s = this.value.s;
        if (v == undefined) v = this.value.v;
        if (a == undefined) a = this.value.a;

        h *= 360;
        var R, G, B, X, C;
        h = (h % 360) / 60;
        C = v * s;
        X = C * (1 - Math.abs(h % 2 - 1));
        R = G = B = v - C;

        h = ~~h;
        R += [C, X, 0, 0, X, C][h];
        G += [X, C, C, X, 0, 0][h];
        B += [0, 0, X, C, C, X][h];

        return {
            r: R,
            g: G,
            b: B,
            a: a
        };
        
    },
    toHex: function(h, s, v, a) {
        var rgb = this.toRGB(h, s, v, a);
        return '#' + ((1 << 24) | (parseInt(rgb.r * 255) << 16) | (parseInt(rgb.g * 255) << 8) | parseInt(rgb.b * 255)).toString(16).substr(1);
    },
    toHSL: function(h, s, v, a) {
        if (h == undefined) h = this.value.h;
        if (s == undefined) s = this.value.s;
        if (v == undefined) v = this.value.v;
        if (a == undefined) a = this.value.a;

        var H = h,
            L = (2 - s) * v,
            S = s * v;
        if (L > 0 && L <= 1) {
            S /= L;
        } else {
            S /= 2 - L;
        }
        L /= 2;
        if (S > 1) {
            S = 1;
        }
        return {
            h: isNaN(H) ? 0 : H,
            s: isNaN(S) ? 0 : S,
            l: isNaN(L) ? 0 : L,
            a: isNaN(a) ? 0 : a,
            valid: true
        };
    },
    toAlias: function(r, g, b, a) {
        var rgb = this.toHex(r, g, b, a);
        for (var alias in this.colors) {
            if (this.colors[alias] == rgb) {
                return alias;
            }
        }
        return false;
    },
    RGBtoHSV: function(r, g, b, a) {

        var H, S, V, C;
        V = Math.max(r, g, b);
        C = V - Math.min(r, g, b);
        H = (C === 0 ? null :
            V === r ? (g - b) / C :
            V === g ? (b - r) / C + 2 :
            (r - g) / C + 4
        );
        H = ((H + 360) % 6) * 60 / 360;
        S = C === 0 ? 0 : C / V;
        return {
            h: this._sanitizeNumber(H),
            s: S,
            v: V,
            a: this._sanitizeNumber(a),
            valid: true
        };
    },
    HueToRGB: function(p, q, h) {
        if (h < 0) {
            h += 1;
        } else if (h > 1) {
            h -= 1;
        }
        if ((h * 6) < 1) {
            return p + (q - p) * h * 6;
        } else if ((h * 2) < 1) {
            return q;
        } else if ((h * 3) < 2) {
            return p + (q - p) * ((2 / 3) - h) * 6;
        } else {
            return p;
        }
    },
    HSLtoRGB: function(h, s, v, a) {
        if (s < 0) {
            s = 0;
        }
        var q;
        if (v <= 0.5) {
            q = v * (1 + s);
        } else {
            q = v + s - (v * s);
        }

        var p = 2 * v - q;

        var tr = h + (1 / 3);
        var tg = h;
        var tb = h - (1 / 3);

        var r = this.HueToRGB(p, q, tr);
        var g = this.HueToRGB(p, q, tg);
        var b = this.HueToRGB(p, q, tb);
        return [r, g, b, this._sanitizeNumber(a)];
    },
    toString: function(format) {

        if (! format && this.value.a == 1) {
            format = "hex"
        } else if (! format) {
            format = "rgba"
        }
        switch (format) {
            case 'rgb':
                {

                    var rgb = this.toRGB();
                    return 'rgb(' + Math.round(rgb.r * 255) + ', ' + Math.round(rgb.g * 255) + ', ' + Math.round(rgb.b * 255) + ')';
                }
                break;
            case 'rgba':
                {

                    var rgb = this.toRGB();
                    return 'rgba(' + Math.round(rgb.r * 255) + ', ' + Math.round(rgb.g * 255) + ', ' + Math.round(rgb.b * 255) + ', ' + rgb.a.toFixed(2) + ')';
                }
                break;
            case 'hsl':
                {
                    var hsl = this.toHSL();
                    return 'hsl(' + Math.round(hsl.h * 360) + ', ' + Math.round(hsl.s * 100) + '%, ' + Math.round(hsl.l * 100) + '%)';
                }
                break;
            case 'hsla':
                {
                    var hsl = this.toHSL();
                    return 'hsla(' + Math.round(hsl.h * 360) + ', ' + Math.round(hsl.s * 100) + '%, ' + Math.round(hsl.l * 100) + '%, ' + hsl.a + ')';
                }
                break;
            case 'hex':
                {
                    return this.toHex();
                }
                break;
            case 'alias':
                return this.toAlias() || this.toString();
            default:
                {
                    return false;
                }
                break;
        }
    },
    // a set of RE's that can match strings and generate color tuples.
    // from John Resig color plugin
    // https://github.com/jquery/jquery-color/
    stringParsers: [{
        re: /^#([a-fA-F0-9]{2})([a-fA-F0-9]{2})([a-fA-F0-9]{2})$/,
        format: 'hex',
        parse: function(execResult) {
            return [
                parseInt(execResult[1], 16) / 255,
                parseInt(execResult[2], 16) / 255,
                parseInt(execResult[3], 16) / 255,
                1
            ];
        }
    }, {
        re: /^#([a-fA-F0-9])([a-fA-F0-9])([a-fA-F0-9])$/,
        format: 'hex',
        parse: function(execResult) {
            return [
                parseInt(execResult[1] + execResult[1], 16) / 255,
                parseInt(execResult[2] + execResult[2], 16) / 255,
                parseInt(execResult[3] + execResult[3], 16) / 255,
                1
            ];
        }
    }, {
        re: /^rgb\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*?\)$/,
        format: 'rgb',
        parse: function(execResult) {
            return [
                execResult[1] / 255,
                execResult[2] / 255,
                execResult[3] / 255,
                1
            ];
        }
    }, {
        re: /^rgb\(\s*(\d+(?:\.\d+)?)\%\s*,\s*(\d+(?:\.\d+)?)\%\s*,\s*(\d+(?:\.\d+)?)\%\s*?\)$/,
        format: 'rgb',
        parse: function(execResult) {
            return [
                execResult[1] / 100,
                execResult[2] / 100,
                execResult[3] / 100,
                1
            ];
        }
    }, {
        re: /^rgba\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*(?:,\s*(\d+(?:\.\d+)?)\s*)?\)$/,
        format: 'rgba',
        parse: function(execResult) {
            return [
                execResult[1] / 255,
                execResult[2] / 255,
                execResult[3] / 255,
                execResult[4]
            ];
        }
    }, {
        re: /^rgba\(\s*(\d+(?:\.\d+)?)\%\s*,\s*(\d+(?:\.\d+)?)\%\s*,\s*(\d+(?:\.\d+)?)\%\s*(?:,\s*(\d+(?:\.\d+)?)\s*)?\)$/,
        format: 'rgba',
        parse: function(execResult) {
            console.log(execResult[4])
            return [
                execResult[1] / 100,
                execResult[2] / 100,
                execResult[3] / 100,
                execResult[4]
            ];
        }
    }, {
        re: /^hsl\(\s*(\d+(?:\.\d+)?)\s*,\s*(\d+(?:\.\d+)?)\%\s*,\s*(\d+(?:\.\d+)?)\%\s*?\)$/,
        format: 'hsl',
        parse: function(execResult) {
            return [
                execResult[1] / 360,
                execResult[2] / 100,
                execResult[3] / 100,
                execResult[4]
            ];
        }
    }, {
        re: /^hsla\(\s*(\d+(?:\.\d+)?)\s*,\s*(\d+(?:\.\d+)?)\%\s*,\s*(\d+(?:\.\d+)?)\%\s*(?:,\s*(\d+(?:\.\d+)?)\s*)?\)$/,
        format: 'hsla',
        parse: function(execResult) {
            return [
                execResult[1] / 360,
                execResult[2] / 100,
                execResult[3] / 100,
                execResult[4]
            ];
        }
    }, {
        //predefined color name
        re: /^([a-z]{3,})$/,
        format: 'alias',
        parse: function(execResult) {
            var hexval = this.colorNameToHex(execResult[0]);
            if (! hexval) return false;
            var match = this.stringParsers[0].re.exec(hexval),
                values = match && this.stringParsers[0].parse.apply(this, [match]);
            return values;
        }
    }],
    colorNameToHex: function(name) {
        if (typeof this.colors[name.toLowerCase()] !== 'undefined') {
            return this.colors[name.toLowerCase()];
        }
        return false;
    }
};


/**
 * drag plugin made by riccardod@wolfram.com
 */

!function($) {

    /**
     * Constructor to create a new smartselect using the given select.
     * 
     * @param {jQuery} select
     * @param {Object} options
     * @returns {Multiselect}
     */
    function DragAround(handle, options) {

        this.options   = $.extend({}, this.defaults, options);
        this.$handle = $(handle);
        this.$dragging = false;
        this.$document = this.$handle[0].ownerDocument
        
        this.buildData()
        this.buildContainer()
        this.buildHandle()
        this.buildEvents()
        this.updatePosition()
    };

    DragAround.prototype = {
        defaults: {},
        constructor: DragAround,
        buildData: function() {
            this.$data = {
                clickX : 0,
                clickY : 0,
                valX:  (this.options.valX  == undefined) ? 0 : parseFloat(this.options.valX),
                minX:  (this.options.minX  == undefined) ? 0 : parseFloat(this.options.minX), 
                maxX:  (this.options.maxX  == undefined) ? 1 : parseFloat(this.options.maxX), 
                stepX: (this.options.stepX == undefined) ? 0 : parseFloat(this.options.stepX),
                valY:  (this.options.valY  == undefined) ? 0 : parseFloat(this.options.valY), 
                minY:  (this.options.minY  == undefined) ? 0 : parseFloat(this.options.minY), 
                maxY:  (this.options.maxY  == undefined) ? 1 : parseFloat(this.options.maxY), 
                stepY: (this.options.stepY == undefined) ? 0 : parseFloat(this.options.stepY)
            };

            this.buildReverse();

        },
        buildReverse: function() {

            this.$data.reverseX = this.$data.minX >= this.$data.maxX;
            if (this.$data.reverseX) {
                $.extend(
                    this.$data, {
                        minX: this.$data.maxX, 
                        maxX: this.$data.minX
                    }
                );
            };

            this.$data.reverseY = this.$data.minY >= this.$data.maxY;
            if (this.$data.reverseY) {
                $.extend(
                    this.$data, {
                        minY: this.$data.maxY, 
                        maxY: this.$data.minY
                    }
                );
            };
        },
        buildContainer: function() {
            this.$container = $(this.options.container || this.$handle.parent());
            this.$button    = $(this.options.button || this.$container);
        },
        buildHandle: function() {
            this.$handle.css({position:"absolute"})
        },
        buildEvents: function() {
            
            $(this.$document)
                .bind("MSPointerUp touchend mouseup", $.proxy(function(event){
                    if (this.$container.is(":visible") && this.$dragging) {
                        this.updateClick(event);
                        this.moveTo();
                        this.endDrag()
                        event.stopPropagation();
                        event.preventDefault();
                    };
                }, this))
                .bind("MSPointerDown touchstart mousedown", $.proxy(function(event){
                    if (this.$container.is(":visible") && ! this.$dragging) {
                        this.updateClick(event);
                        if (this.isClickInside()) {
                            this.moveTo();
                            this.startDrag();
                            event.stopPropagation();
                            event.preventDefault();
                            if (this.$button.is(":input")) this.$button.focus()
                        };
                    }
                }, this))
                .bind("MSPointerMove touchmove mousemove", $.proxy(function(event){
                    if (this.$container.is(":visible") && this.$dragging) {
                        this.updateClick(event);
                        this.moveTo();
                        this.moveDrag();
                        event.stopPropagation();
                        event.preventDefault();
                    };
                }, this));

            if (this.$button.is(":input")) {
                this.$button
                    .on("keydown", $.proxy(function(event){

                        if (/^(13)$/.test(event.keyCode)) {
                            // submit on enter
                            this.$button.closest("form").submit()

                        } else if (/^(37|38|39|40)$/.test(event.keyCode)) {
                            // on arrow left/right do stuff
                            switch(event.keyCode) {
                            case 37:
                                // left key
                                this.setX(this.$data.valX - this.$data.stepX);
                                break;
                            case 39:
                                // right key
                                this.setX(this.$data.valX + this.$data.stepX);
                                break;
                            case 38:
                                // up key
                                this.setY(this.$data.valY - this.$data.stepY);
                                break;
                            case 40:
                                // down key
                                this.setY(this.$data.valY - this.$data.stepY);
                                break;
                            }
                            this.updatePosition();
                            this.moveDrag();
                            event.stopPropagation();
                            event.preventDefault();
                        }
                    }, this));                
            }

        },
        startDrag: function() {
            this.$dragging = true;
            if (this.options.start) this.options.start(
                this.fixedTo(this.$data.valX, this.$data.stepX), 
                this.fixedTo(this.$data.valY, this.$data.stepY) 
            );
        },
        moveDrag: function() {
            if (this.options.move)  this.options.move(
                this.fixedTo(this.$data.valX, this.$data.stepX), 
                this.fixedTo(this.$data.valY, this.$data.stepY)
            );
        },
        endDrag: function() {
            this.$dragging = false;
            if (this.options.end)   this.options.end(
                this.fixedTo(this.$data.valX, this.$data.stepX), 
                this.fixedTo(this.$data.valY, this.$data.stepY)
            );
        },
        isClickInside: function(event) {
            return (
                this.$data.clickX >= 0 &&
                this.$data.clickX <= this.$container.width() &&
                this.$data.clickY >= 0 &&
                this.$data.clickY <= this.$container.height()
            )
        },
        isTouch: function(event){
            return event.type.search('touch') > -1;
        },
        updateClick: function(event) {
            var offset = this.$container.offset();
            if (! this.isTouch(event)) {
                this.$data.clickX = event.pageX - offset.left;
                this.$data.clickY = event.pageY - offset.top;
            } else if (event.originalEvent.touches.length) {
                this.$data.clickX = event.originalEvent.touches[0].pageX - offset.left;
                this.$data.clickY = event.originalEvent.touches[0].pageY - offset.top;
            } else {
                this.$data.clickX = undefined;
                this.$data.clickY = undefined;
            }
        },
        getDecimals: function(num) {
            num = (num + "").split(".")[1];
            if (num) return num.length;
            return 0;
        },
        fixedTo: function(n, step) {

            if (! step) return n

            var a = this.getDecimals(step);
            var b = this.getDecimals(n);

            if (b > a) return n.toFixed(a);
            return n.toFixed(b);
        },
        roundTo: function(n, step) {
            if (step) {
                return Math.round(n / step) * step
            } else {
                return n;
            }
        },
        setX: function(x) {
            this.$data.valX = this.roundTo(
                Math.min(Math.max(this.$data.minX, x), this.$data.maxX),
                this.$data.stepX
                );    
        },
        setY: function(y) {
            this.$data.valY = this.roundTo(
                Math.min(Math.max(this.$data.minY, y), this.$data.maxY),
                this.$data.stepY
                );      
        },
        convertTo:function(val, min, max) {
            return (val - min) / (max - min)
        },
        convertToReverse:function(val, min, max) {
            return min + (max - min) * val
        },
        updatePosition: function() {

            var x = this.convertTo(this.$data.valX, this.$data.minX, this.$data.maxX)
            var y = this.convertTo(this.$data.valY, this.$data.minY, this.$data.maxY)

            if (this.$data.reverseX) x = 1 - x;
            if (this.$data.reverseY) y = 1 - y;

            if (this.options.axis == "x") {
                this.$handle.css({
                    left: (x * 100) + "%"
                })                
            } else if (this.options.axis == "y") {
                this.$handle.css({
                    top:  (y * 100) + "%"
                })                
            } else {
                this.$handle.css({
                    left: (x * 100) + "%",
                    top:  (y * 100) + "%"
                })                
            }
        },
        moveTo: function() {

            if (this.$data.clickX == undefined || this.$data.clickY == undefined) return;

            var x = this.$data.clickX / this.$container.width();
            var y = this.$data.clickY / this.$container.height();

            x = Math.min(Math.max(0, x), 1);
            y = Math.min(Math.max(0, y), 1);

            if (this.$data.reverseX) x = 1 - x;
            if (this.$data.reverseY) y = 1 - y;

            x = this.convertToReverse(x, this.$data.minX, this.$data.maxX)
            y = this.convertToReverse(y, this.$data.minY, this.$data.maxY)

            this.setX(x);
            this.setY(y);

            this.updatePosition();
        }
    };

    $.fn.registerPlugin(DragAround, "dragaround");

}(window.jQuery);


/**
 * slider plugin made by riccardod@wolfram.com
 */
!function($) {

    /**
     * Constructor to create a new smartselect using the given select.
     * 
     * @param {jQuery} select
     * @param {Object} options
     * @returns {Multiselect}
     */
    function SliderControl(original, options) {

        this.options   = $.extend({}, this.defaults, options);
        this.$original = $(original);

        // Build select all if enabled.

        this.buildButton();
        this.buildBar();
        this.buildContainer();

        this.buildHandle();
        this.buildOriginal();
        this.buildTooltip();
            
    };

    SliderControl.prototype = {
        constructor: SliderControl,
        defaults: {
            templates: {
                container: '<div class="slider-drag"/>',
                handle: '<div class="slider-handle-wrapper"><div class="slider-handle"/></div>',
                bar: '<div class="slider-bar"/>',
                button: '<button type="button" class="slider"/>'
            }
        },
        buildContainer: function() {
            this.$container = $(this.options.templates.container);
            this.$button.append(this.$container);
        },
        buildBar: function() {
            this.$bar = $(this.options.templates.bar);
            this.$button.append(this.$bar);
        },
        buildButton: function() {
            this.$button = $(this.options.templates.button);
            if (this.$original.attr("disabled")) {
                this.$button.attr("disabled", this.$original.attr("disabled"))
                this.$button.addClass("disabled")
            }
            this.$original.after(this.$button);
        },
        buildHandle: function() {
            this.$handle = $(this.options.templates.handle);
            this.$container.append(this.$handle);
            if (! this.$original.attr("disabled")) {
                this.$handle.dragaround({
                    container: this.$container,
                    button: this.$button,
                    axis:  "x",
                    valX:  this.$original.val(),
                    minX:  this.$original.attr("min"),
                    maxX:  this.$original.attr("max"),
                    stepX: this.$original.attr("step"),
                    start: $.proxy(function(x, y) {
                        this.$tooltip.options.title = x;
                        this.$tooltip.show()
                    }, this),
                    move: $.proxy(function(x, y) {
                        this.$original.val(x)
                        this.$tooltip.options.title = x;
                        this.$tooltip.show()
                    }, this),
                    end: $.proxy(function(x, y) {
                        this.$original.val(x)
                        this.$tooltip.options.title = x;
                        this.$tooltip.hide()
                        this.$original.change()
                    }, this)
                });
            }

            //this.$handle.dragaround();
        },
        buildOriginal: function() {
            this.$original
                .addClass("slider-original")
                .attr("type", "hidden")
                .attr("style", "display:none")
                .hide();
        },
        buildTooltip: function() {

            this.$tooltip = this.$handle
                .children()
                .tooltip({
                    title: this.$original.val(),
                    trigger: "hover",
                    animation: false
                })
                .data('bs.tooltip');

            this.$button.on("blur", $.proxy(function() {
                this.$tooltip.hide()
            }, this))
        }

    };

    $.fn.registerPlugin(SliderControl, "slidercontrol");

}(window.jQuery);


/**
 * bootstrap-smartselect.js
 * https://github.com/davidstutz/bootstrap-smartselect
 *
 * Copyright 2012 - 2014 David Stutz
 *
 * Dual licensed under the BSD-3-Clause and the Apache License, Version 2.0.
 *
 * edited by riccardod@wolfram.com
 */
!function($) {

    var SELECT_COUNTER = 0;

    "use strict";// jshint ;_;

    if (typeof ko !== 'undefined' && ko.bindingHandlers && !ko.bindingHandlers.smartselect) {
        ko.bindingHandlers.smartselect = {

            init: function (element, valueAccessor, allBindingsAccessor, viewModel, bindingContext) {

                var listOfSelectedItems = allBindingsAccessor().selectedOptions,
                    config = ko.utils.unwrapObservable(valueAccessor());

                $(element).smartselect(config);

                if (isObservableArray(listOfSelectedItems)) {
                    // Subscribe to the selectedOptions: ko.observableArray
                    listOfSelectedItems.subscribe(function (changes) {
                        var addedArray = [], deletedArray = [];
                        changes.forEach(function (change) {
                            switch (change.status) {
                                case 'added':
                                    addedArray.push(change.value);
                                    break;
                                case 'deleted':
                                    deletedArray.push(change.value);
                                    break;
                            }
                        });
                        if (addedArray.length > 0) {
                            $(element).smartselect('select', addedArray);
                        };
                        if (deletedArray.length > 0) {
                            $(element).smartselect('deselect', deletedArray);
                        };
                    }, null, "arrayChange");
                }
            },

            update: function (element, valueAccessor, allBindingsAccessor, viewModel, bindingContext) {

                var listOfItems = allBindingsAccessor().options,
                    ms = $(element).data('smartselect'),
                    config = ko.utils.unwrapObservable(valueAccessor());

                if (isObservableArray(listOfItems)) {
                    // Subscribe to the options: ko.observableArray incase it changes later
                    listOfItems.subscribe(function (theArray) {
                        $(element).smartselect('rebuild');
                    });
                }

                if (!ms) {
                    $(element).smartselect(config);
                }
                else {
                    ms.updateOriginalOptions();
                }
            }
        };
    }

    function isObservableArray(obj) {
        return ko.isObservable(obj) && !(obj.destroyAll === undefined);
    }

    /**
     * Constructor to create a new smartselect using the given select.
     * 
     * @param {jQuery} select
     * @param {Object} options
     * @returns {SmartSelect}
     */
    function SmartSelect(select, options) {

        this.options = this.mergeOptions(options);
        this.$select = $(select);


        this.$id = "select-dropdown-" + SELECT_COUNTER;

        SELECT_COUNTER++;

        // Initialization.
        // We have to clone to create a new reference.
        this.originalOptions = this.$select.clone()[0].options;
        this.searchTimeout = null;

        this.options.multiple = this.$select.attr('multiple') === "multiple";
        this.options.onChange = $.proxy(this.options.onChange, this);
        this.options.onDropdownShow = $.proxy(this.options.onDropdownShow, this);
        this.options.onDropdownHide = $.proxy(this.options.onDropdownHide, this);
        this.options.onMouseClick   = $.proxy(this.options.onMouseClick, this);
        this.options.onKeyDown      = $.proxy(this.options.onKeyDown, this);

        // Build select all if enabled.
        this.buildContainer();
        this.buildButton();
        this.buildSelectAll();
        this.buildDropdown();
        this.buildDropdownOptions();
        this.buildFilter();
        this.buildDropdownCaret();

        this.removeSelect();
        
        this.updateButtonText();
        this.updateSelectAll();
        
        
    };

    SmartSelect.prototype = {

        defaults: {
            /**
             * Default text function will either print 'None selected' in case no
             * option is selected or a list of the selected options up to a length of 3 selected options.
             * 
             * @param {jQuery} options
             * @param {jQuery} select
             * @returns {String}
             */
            buttonText: function(options, select) {
                if (options.length === 0) {
                    return this.nonSelectedText;
                }
                else {
                    if (options.length > this.numberDisplayed) {
                        return options.length + ' ' + this.nSelectedText;
                    }
                    else {
                        var selected = '';
                        options.each(function() {
                            var label = ($(this).attr('label') !== undefined) ? $(this).attr('label') : $(this).html();

                            selected += label + ', ';
                        });
                        return selected.substr(0, selected.length - 2);
                    }
                }
            },
            /**
             * Create a label.
             * 
             * @param {jQuery} element
             * @returns {String}
             */
            label: function(element){
                return $(element).attr('label') || $(element).html();
            },
            /**
             * Triggered on change of the smartselect.
             * Not triggered when selecting/deselecting options manually.
             * 
             * @param {jQuery} option
             * @param {Boolean} checked
             */
            onChange : function(option, checked) {
                //console.log("change")
            },
            onMouseClick: function(event) {
                //console.log("click")
                event.preventDefault()
            },
            onKeyDown: function(event) {
                //console.log("down")

            },
            /**
             * Triggered when the dropdown is shown.
             * 
             * @param {jQuery} event
             */
            onDropdownShow: function(event) {
                //console.log("dropdown show");
                this.$search.val("");
                this.refreshFilter();
                this.showFilter();
            },
            /**
             * Triggered when the dropdown is hidden.
             * 
             * @param {jQuery} event
             */
            onDropdownHide: function(event) {
                //console.log("dropdown hide")
                this.hideFilter()
            },
            showInput:false,
            dropRight: false,
            selectedClass: 'active',
            buttonWidth: 'auto',
            // Maximum height of the dropdown menu.
            // If maximum height is exceeded a scrollbar will be displayed.
            maxHeight: false,
            checkboxName: null,
            includeSelectAllOption: false,
            includeSelectAllIfMoreThan: 0,
            selectAllText: ' Select all',
            selectAllValue: 'smartselect-all',
            showFiltering: false,
            filterPlaceholder: 'Search',
            // possible options: 'text', 'value', 'both'
            preventInputChangeEvent: false,
            nonSelectedText: 'None selected',
            nSelectedText: 'selected',
            numberDisplayed: 3,
            templates: {
                caret: '<i class="icon-dropdown text-muted form-control-feedback select-caret"></i>',
                container: '<div class="select"></div>',
                button: '<button type="button" class="smartselect dropdown-toggle"/>',
                ul: '<ul class="smartselect-dropdown dropdown-menu"></ul>',
                filter: '<li class="has-feedback"><input class="form-control select-search" type="search"><i class="glyphicon glyphicon-search icon-search form-control-feedback"></i></li>',
                li: '<li><a href="javascript:void(0);"><label></label></a></li>',
                divider: '<li class="divider"></li>',
                liGroup: '<li><label class="smartselect-group"></label></li>'
            }
        },

        constructor: SmartSelect,

        removeSelect: function() {
            this.$select.hide().addClass("select-hidden");
        },

        /**
         * Builds the container of the smartselect.
         */
        buildContainer: function() {
            this.$container = $(this.options.templates.container)
                .attr("id", this.$id)
                .on('show.bs.dropdown', this.options.onDropdownShow)
                .on('hide.bs.dropdown', this.options.onDropdownHide)
                .on('click.bs.dropdown', this.options.onMouseClick);

            this.$select.after(this.$container);

            this.$container.on('keydown', $.proxy(function(event) {

                if (! this.$button.is(":visible")) return;

                event.stopPropagation();   

                if (/^(9|27)$/.test(event.keyCode)) {
                    // Close on tab or escape.
                    if (this.$container.hasClass('open')) this.$button.trigger("click.bs.dropdown");
                    if (/^(9)$/.test(event.keyCode)) this.$button.focus();
                    return;

                } else if (/^(32|13)$/.test(event.keyCode)) {
                    // Selecting on space|enter

                    // on space with search focused don't do anythin
                    if (event.keyCode === 32 && this.$search.is(':focus')) return;

                    // on enter with search active is useless
                    if (this.$filter.is('.active')) {
                        event.stopPropagation();
                        event.preventDefault();
                        return
                    };

                    if (this.$container.hasClass('open')) {
                        this.$container.find("li.active input").change();
                        this.$button.trigger("click.bs.dropdown");
                    }

                    return;

                } else if (/^(38|40)$/.test(event.keyCode)) {

                    // prevent default un up and down over an input
                    event.preventDefault()

                    if (! this.$container.hasClass('open')) {
                        // This will open the tab
                        this.$button.trigger("click.bs.dropdown");
                        return;
                    }

                    var $items = $(this.$container).find("li:not(.divider):visible");

                    if (!$items.length) {
                        return;
                    }

                    var index = $items.index($items.filter('.active'));

                    // Navigation up.
                    if (event.keyCode === 38) {
                        if (index > 0) {
                            index--;
                        } else {
                            if(this.$container.hasClass('open')) {
                                this.$button
                                    .focus()
                                    .trigger("click.bs.dropdown");                                
                            }
                            return;                            
                        }
                    }



                    // Navigate down.
                    else if (event.keyCode === 40 && index < $items.length - 1) {
                        index++;
                    }
                    else if (!~index) {
                        index = 0;
                    }

                    $items.removeClass("active").find("input").removeAttr("checked")

                    var $current = $items.eq(index);



                    $current.addClass("active")

                    var $checkbox = $current.find('input[type!="search"]');
                    $checkbox.prop("checked", !$checkbox.prop("checked"));

                    if (this.$filter.hasClass("active")) {
                        this.$search.filter(":not(:focus)").focus();
                    } else if (! this.$search.val()) {
                        this.$button.focus();
                    }
                    

                } else if (/^(37|39)$/.test(event.keyCode) && ! this.$filter.is(":visible")) {

                    // on key left, key right with navigation down, do nothing

                } else if (/^(16|17|18|20|91)$/.test(event.keyCode)) {
                    // on shift, command do nothing
                    return ;

                } else if (this.$search) {

                    if (!String.fromCharCode(event.keyCode).trim()) return;

                    // activate the search only on more than 10 elements

                    if ($(this.$container).find("li:not(.divider)").length <= 11) return;

                    // anything else will open the search
                    if (!this.$container.hasClass('open')) {
                        // open the dropdown
                        this.$button.trigger("click.bs.dropdown");
                    };

                    // focus if not
                    this.$filter.filter(":not(:visible)").show();
                    this.$search.filter(":not(:focus)").focus();

                    // Selecting on enter/space
                    if (/^(13)$/.test(event.keyCode)) {
                        event.stopPropagation();
                        event.preventDefault();
                        return
                    };

                    clearTimeout(this.searchTimeout);

                    this.searchTimeout = this.asyncFunction(
                        $.proxy(this.refreshFilter, this), 
                        100, 
                        this
                    );
                }
                
            }, this));




        },

        buildDropdownCaret: function () {
            /* we don't need to attach show.bs.dropdown events here */

            this.$caret = $(this.options.templates.caret)
                .attr("data-target", "#" + this.$id)
                .attr("data-toggle", "dropdown")
                .attr("style", "cursor:pointer")

            this.$button.after(this.$caret)

            this.$caret.closest(".form-group").addClass("has-feedback")

        },

        /**
         * Builds the button of the smartselect.
         */
        buildButton: function() {
            this.$button = $(this.options.templates.button)

            for (var i = 0; i < this.$select[0].attributes.length; i++) {
                var a = this.$select[0].attributes[i];
                if (! /^(value|name|id|type)$/.test(a.name)) {
                    this.$button.attr(a.name, a.value);
                }
            }

            this.$button
                .attr("data-target", "#" + this.$id)
                .attr("data-toggle", "dropdown")
                .off("keydown.bs.dropdown.data-api")
                .closest(".form-group")
                .addClass("has-feedback");

            // Adopt active state.
            if (this.$select.prop('disabled')) {
                this.disable();
            }
            else {
                this.enable();
            }

            // Manually add button width if set.
            if (this.options.buttonWidth && this.options.buttonWidth !== 'auto') {
                this.$button.css({
                    'width' : this.options.buttonWidth
                });
            }

            // Keep the tab index from the select.
            var tabindex = this.$select.attr('tabindex');
            if (tabindex) {
                this.$button.attr('tabindex', tabindex);
            }




            this.$container.prepend(this.$button);


        },

        /**
         * Builds the ul representing the dropdown menu.
         */
        buildDropdown: function() {

            // Build ul.
            this.$ul = $(this.options.templates.ul);
            

            if (this.options.dropRight) {
                this.$ul.addClass('pull-right');
            }

            // Set max height of dropdown menu to activate auto scrollbar.
            if (this.options.maxHeight) {
                // TODO: Add a class for this option to move the css declarations.
                this.$ul.css({
                    'max-height': this.options.maxHeight + 'px',
                    'overflow-y': 'auto',
                    'overflow-x': 'hidden'
                });
            }

            this.$container.append(this.$ul);
        },

        /**
         * Build the dropdown options and binds all nessecary events.
         * Uses createDivider and createOptionValue to create the necessary options.
         */
        buildDropdownOptions: function() {

            

            this.$select.children().each($.proxy(function(index, element) {
                
                // Support optgroups and options without a group simultaneously.
                var tag = $(element).prop('tagName')
                    .toLowerCase();

                if (tag === 'optgroup') {
                    this.createOptgroup(element);
                }
                else if (tag === 'option') {

                    if ($(element).data('role') === 'divider') {
                        this.createDivider();
                    }
                    else {
                        this.createOptionValue(element);
                    }

                }
                
                // Other illegal tags will be ignored.
            }, this));

            // Bind the change event on the dropdown elements.
            $('li input', this.$ul).on('change', $.proxy(function(event) {
                var $target = $(event.target);

                var checked = $target.prop('checked') || false;
                var isSelectAllOption = $target.val() === this.options.selectAllValue;

                // Apply or unapply the configured selected class.
                if (this.options.selectedClass) {
                    if (checked) {
                        $target.parents('li')
                            .addClass(this.options.selectedClass);
                    }
                    else {
                        $target.parents('li')
                            .removeClass(this.options.selectedClass);
                    }
                }

                // Get the corresponding option.
                var value = $target.val();
                var $option = this.getOptionByValue(value);

                var $optionsNotThis = $('option', this.$select).not($option);
                var $checkboxesNotThis = $('input', this.$container).not($target);

                if (isSelectAllOption) {
                    var values = [];
                    
                    // Select the visible checkboxes except the "select-all" and possible divider.
                    var availableInputs = $('li input[value!="' + this.options.selectAllValue + '"][data-role!="divider"]', this.$ul).filter(':visible');
                    for (var i = 0, j = availableInputs.length; i < j; i++) {
                        values.push(availableInputs[i].value);
                    }

                    if (checked) {
                        this.select(values);
                    }
                    else {
                        this.deselect(values);
                    }
                }

                if (checked) {
                    $option.prop('selected', true);

                    if (this.options.multiple) {
                        // Simply select additional option.
                        $option.prop('selected', true);
                    }
                    else {
                        // Unselect all other options and corresponding checkboxes.
                        if (this.options.selectedClass) {
                            $($checkboxesNotThis).parents('li').removeClass(this.options.selectedClass);
                        }

                        $($checkboxesNotThis).prop('checked', false);
                        $optionsNotThis.prop('selected', false);

                        // It's a single selection, so close.

                        this.$button.trigger("click.bs.dropdown")
                    }

                    if (this.options.selectedClass === "active") {
                        $optionsNotThis.parents("a").css("outline", "");
                    }
                }
                else {
                    // Unselect option.
                    $option.prop('selected', false);
                }

                this.$select.change();
                this.options.onChange($option, checked);
                
                this.updateButtonText();
                this.updateSelectAll();

                if(this.options.preventInputChangeEvent) {
                    return false;
                }
            }, this));

            $('li a', this.$ul).on('touchstart click', function(event) {
                event.stopPropagation();
                $(event.target).find("input").change();
            });

        },

        

        /**
         * Create an option using the given select option.
         * 
         * @param {jQuery} element
         */
        createOptionValue: function(element) {
            if ($(element).is(':selected')) {
                $(element).prop('selected', true);
            }

            // Support the label attribute on options.
            var label = this.options.label(element);
            var value = $(element).val();
            var inputType = this.options.multiple ? "checkbox" : "radio";

            var $li = $(this.options.templates.li);

            
            $('label', $li).append('<input type="' + inputType + (this.options.checkboxName ? ('" name="' + this.options.checkboxName) : "") + '" />');

            var selected = $(element).prop('selected') || false;
            var $checkbox = $('input', $li);
            $checkbox.val(value);

            if (value === this.options.selectAllValue) {
                $checkbox.parent().parent()
                    .addClass('smartselect-all');
            }

            if (this.options.showInput) {
                $('label', $li).addClass(inputType);
                $('label', $li).append(' <span class="'+inputType+'-label label-text">' + label + '</span>');          
            } else {
                $('label', $li).append(' <span class="label-text">' + label + '</span>')
                    .addClass("dropdown-label");
                $checkbox.hide()
            }

            $li.attr("data-label", label)

            this.$ul.append($li);

            if ($(element).is(':disabled')) {
                $checkbox.attr('disabled', 'disabled')
                    .prop('disabled', true)
                    .parents('li')
                    .addClass('disabled');
            }

            $checkbox.prop('checked', selected);

            if (selected && this.options.selectedClass) {
                $checkbox.parents('li')
                    .addClass(this.options.selectedClass);
            }
        },

        /**
         * Creates a divider using the given select option.
         * 
         * @param {jQuery} element
         */
        createDivider: function(element) {
            var $divider = $(this.options.templates.divider);
            this.$ul.append($divider);
        },

        /**
         * Creates an optgroup.
         * 
         * @param {jQuery} group
         */
        createOptgroup: function(group) {
            var groupName = $(group).prop('label');

            // Add a header for the group.
            var $li = $(this.options.templates.liGroup);
            $('label', $li).text(groupName);

            this.$ul.append($li);

            if ($(group).is(':disabled')) {
                $li.addClass('disabled');
            }

            // Add the options of the group.
            $('option', group).each($.proxy(function(index, element) {
                this.createOptionValue(element);
            }, this));
        },

        /**
         * Build the selct all.
         * Checks if a select all ahs already been created.
         */
        buildSelectAll: function() {
            var alreadyHasSelectAll = this.hasSelectAll();
            
            if (!alreadyHasSelectAll && this.options.includeSelectAllOption && this.options.multiple
                    && $('option[data-role!="divider"]', this.$select).length > this.options.includeSelectAllIfMoreThan) {
                
                // Check whether to add a divider after the select all.
                if (this.options.includeSelectAllDivider) {
                    this.$select.prepend('<option value="" disabled="disabled" data-role="divider">');
                }
                
                this.$select.prepend('<option value="' + this.options.selectAllValue + '">' + this.options.selectAllText + '</option>');
            }
        },

        refreshFilter: function() {
            $.each($('li', this.$ul), $.proxy(function(index, element) {
                var value = $('input', element).val();
                var label = $(element).attr("data-label");

                if (value !== this.options.selectAllValue && label) {
                    // by default lets assume that element is not
                    // interesting for this search

                    var result = label;
                    
                    if (this.$search.val().trim() !== "") {
                        var and_query = "";
                        var or_query  = "";

                        $.each(
                            this.$search.val().trim()
                            .replace(/[\-\[\]{}()*+?.,\\\^$|#]/g, '\\$&')
                            .split(/\s{1,}/g), function(index, value) {
                                and_query += '(?=.*'+value+')'

                                if (! or_query == "") or_query += "|"
                                or_query += value
                            });

                        var showElement = label.match(new RegExp(and_query, 'ig'));
                        
                        if (showElement) {
                            result = label.replace(new RegExp('(' + or_query + ')', 'ig'), function ($1, match) {
                                return '<strong>' + match + '</strong>'
                            });

                        }

                    } else {
                        
                        var showElement = true;
                    };

                    if (showElement) {
                        $(element).show().find(".label-text").html(result);
                    }
                    else {
                        $(element).hide();
                    }
                }
            }, this));
        },

        /**
         * Builds the filter.
         */
        buildFilter: function() {

            // Build filter if filtering OR case insensitive filtering is enabled and the number of options exceeds (or equals) enableFilterLength.

            this.$filter = $(this.options.templates.filter);
            this.$search = this.$filter.find('[type=search]')
                .on('click', $.proxy(function(event) {
                    event.stopPropagation();
                    this.$container.find("li.active").removeClass("active")
                    this.$filter.addClass("active")
                }, this));

            $('input', this.$filter).attr('placeholder', this.options.filterPlaceholder);
            this.$ul.prepend(this.$filter);

            this.$filter.val("").on('click', function(event) {
                event.stopPropagation();
            });

            this.showFilter()
            
        },

        showFilter: function() {

            if (this.options.showFiltering) {
                this.$filter.show()
                this.$search.val("")
            } else {
                this.$filter.hide()
            }
        },

        hideFilter: function() {
            
        },

        /**
         * Unbinds the whole plugin.
         */
        destroy: function() {
            this.$container.remove();
            this.$select.show();
            this.$select.data('smartselect', null);
        },

        /**
         * Refreshs the smartselect based on the selected options of the select.
         */
        refresh: function() {
            $('option', this.$select).each($.proxy(function(index, element) {
                var $input = $('li input', this.$ul).filter(function() {
                    return $(this).val() === $(element).val();
                });

                if ($(element).is(':selected')) {
                    $input.prop('checked', true);

                    if (this.options.selectedClass) {
                        $input.parents('li')
                            .addClass(this.options.selectedClass);
                    }
                }
                else {
                    $input.prop('checked', false);

                    if (this.options.selectedClass) {
                        $input.parents('li')
                            .removeClass(this.options.selectedClass);
                    }
                }

                if ($(element).is(":disabled")) {
                    $input.attr('disabled', 'disabled')
                        .prop('disabled', true)
                        .parents('li')
                        .addClass('disabled');
                }
                else {
                    $input.prop('disabled', false)
                        .parents('li')
                        .removeClass('disabled');
                }

                $input.attr("data-label", $(element).text())

            }, this));

            this.updateButtonText();
            this.updateSelectAll();
        },

        /**
         * Select all options of the given values.
         * 
         * @param {Array} selectValues
         */
        select: function(selectValues) {
            if(!$.isArray(selectValues)) {
                selectValues = [selectValues];
            }

            for (var i = 0; i < selectValues.length; i++) {
                var value = selectValues[i];

                var $option = this.getOptionByValue(value);
                var $checkbox = this.getInputByValue(value);

                if (this.options.selectedClass) {
                    $checkbox.parents('li')
                        .addClass(this.options.selectedClass);
                }

                $checkbox.prop('checked', true);
                $option.prop('selected', true);
            }

            this.updateButtonText();
        },

        /**
         * Clears all selected items
         * 
         */
        clearSelection: function () {

            var selected = this.getSelected();

            if (selected.length) {

                var arry = [];

                for (var i = 0; i < selected.length; i = i + 1) {
                    arry.push(selected[i].value);
                }

                this.deselect(arry);
                this.$select.change();
            }
        },

        /**
         * Deselects all options of the given values.
         * 
         * @param {Array} deselectValues
         */
        deselect: function(deselectValues) {
            if(!$.isArray(deselectValues)) {
                deselectValues = [deselectValues];
            }

            for (var i = 0; i < deselectValues.length; i++) {

                var value = deselectValues[i];

                var $option = this.getOptionByValue(value);
                var $checkbox = this.getInputByValue(value);

                if (this.options.selectedClass) {
                    $checkbox.parents('li')
                        .removeClass(this.options.selectedClass);
                }

                $checkbox.prop('checked', false);
                $option.prop('selected', false);
            }

            this.updateButtonText();
        },

        /**
         * Rebuild the plugin.
         * Rebuilds the dropdown, the filter and the select all option.
         */
        rebuild: function() {
            this.$ul.html('');

            // Remove select all option in select.
            $('option[value="' + this.options.selectAllValue + '"]', this.$select).remove();

            // Important to distinguish between radios and checkboxes.
            this.options.multiple = this.$select.attr('multiple') === "multiple";

            this.buildSelectAll();
            this.buildDropdownOptions();
            this.buildFilter();
            
            this.updateButtonText();
            this.updateSelectAll();
        },

        /**
         * The provided data will be used to build the dropdown.
         * 
         * @param {Array} dataprovider
         */
        dataprovider: function(dataprovider) {
            var optionDOM = "";
            dataprovider.forEach(function (option) {
                optionDOM += '<option value="' + option.value + '">' + option.label + '</option>';
            });

            this.$select.html(optionDOM);
            this.rebuild();
        },

        /**
         * Enable the smartselect.
         */
        enable: function() {
            this.$select.prop('disabled', false);
            this.$button.prop('disabled', false)
                .removeClass('disabled');
        },

        /**
         * Disable the smartselect.
         */
        disable: function() {
            this.$select.prop('disabled', true);
            this.$button.prop('disabled', true)
                .addClass('disabled');
        },

        /**
         * Set the options.
         * 
         * @param {Array} options
         */
        setOptions: function(options) {
            this.options = this.mergeOptions(options);
        },

        /**
         * Merges the given options with the default options.
         * 
         * @param {Array} options
         * @returns {Array}
         */
        mergeOptions: function(options) {
            return $.extend(true, {}, this.defaults, options);
        },
        
        /**
         * Checks whether a select all option is present.
         * 
         * @returns {Boolean}
         */
        hasSelectAll: function() {
            return $('option[value="' + this.options.selectAllValue + '"]', this.$select).length > 0;
        },
        
        /**
         * Updates the select all option based on the currently selected options.
         */
        updateSelectAll: function() {
            if (this.hasSelectAll()) {
                var selected = this.getSelected();
                
                if (selected.length === $('option:not([data-role=divider])', this.$select).length - 1) {
                    this.select(this.options.selectAllValue);
                }
                else {
                    this.deselect(this.options.selectAllValue);
                }
            }
        },
        
        /**
         * Update the button text and its title based on the currently selected options.
         */
        updateButtonText: function() {
            var options = this.getSelected();
            
            // First update the displayed button text.
            // next update the title attribute of the button.

            if (this.$button.is("input")) {
                this.$button
                    .val(this.options.buttonText(options, this.$select))

            } else {
                var inner = this.$button.find(".select-text")
                if (inner.length == 0) {
                    inner = $('<span class="select-text"></span>');
                    this.$button.append(inner)
                }
                inner.text(this.options.buttonText(options, this.$select))
            }

            this.$button
                .focus();
        },

        /**
         * Get all selected options.
         * 
         * @returns {jQUery}
         */
        getSelected: function() {
            return $('option[value!="' + this.options.selectAllValue + '"]:selected', this.$select).filter(function() {
                return $(this).prop('selected');
            });
        },

        /**
         * Gets a select option by its value.
         * 
         * @param {String} value
         * @returns {jQuery}
         */
        getOptionByValue: function (value) {

            var options = $('option', this.$select);
            var valueToCompare = value.toString();

            for (var i = 0; i < options.length; i = i + 1) {
                var option = options[i];
                if (option.value === valueToCompare) {
                    return $(option);
                }
            }
        },

        /**
         * Get the input (radio/checkbox) by its value.
         * 
         * @param {String} value
         * @returns {jQuery}
         */
        getInputByValue: function (value) {

            var checkboxes = $('li input', this.$ul);
            var valueToCompare = value.toString();

            for (var i = 0; i < checkboxes.length; i = i + 1) {
                var checkbox = checkboxes[i];
                if (checkbox.value === valueToCompare) {
                    return $(checkbox);
                }
            }
        },

        /**
         * Used for knockout integration.
         */
        updateOriginalOptions: function() {
            this.originalOptions = this.$select.clone()[0].options;
        },

        asyncFunction: function(callback, timeout, self) {
            var args = Array.prototype.slice.call(arguments, 3);
            return setTimeout(function() {
                callback.apply(self || window, args);
            }, timeout);
        }
    };

    $.fn.registerPlugin(SmartSelect, "smartselect");


}(window.jQuery);


/**
 * color picker plugin made by riccardod@wolfram.com
 */

!function($) {

    function ColorSetter(original, options) {
        this.options   = $.extend({}, this.defaults, options);
        this.$container = $(original);

        this.buildData();
        this.buildPicker();
    };

    ColorSetter.prototype = {
        constructor: ColorSetter,
        defaults: {
            templates: {
                picker: 
                '<div class="color-setter">\
                    <div class="color-setter-controls-base">\
                        <div class="color-setter-controls-hsl">\
                            <div class="color-setter-saturation">\
                                <div class="color-setter-handle"/>\
                            </div>\
                            <div class="color-setter-hue">\
                                <div class="color-setter-handle"/>\
                            </div>\
                        </div>\
                        <div class="color-setter-controls-alpha">\
                            <div class="color-setter-alpha">\
                                <div class="color-setter-handle"/>\
                            </div>\
                            <div class="color-setter-comparison">\
                                <div class="color-setter-previous"/>\
                                <div class="color-setter-current update-current"/>\
                            </div>\
                        </div>\
                    </div>\
                    <div class="color-setter-controls-advanced">\
                        <div class="color-control-group">\
                            <div class="color-control">\
                                <label class="control-label">R</label>\
                                <input type="number" min="0" max="255" step="1" class="form-control input-sm" data-component="r"/>\
                            </div>\
                            <div class="color-control">\
                                <label class="control-label">G</label>\
                                <input type="number" min="0" max="255" step="1" class="form-control input-sm" data-component="g"/>\
                            </div>\
                            <div class="color-control">\
                                <label class="control-label">B</label>\
                                <input type="number" min="0" max="255" step="1" class="form-control input-sm" data-component="b"/>\
                            </div>\
                        </div>\
                        <div class="color-control-group">\
                            <div class="color-control">\
                                <label class="control-label">H</label>\
                                <input type="number" min="0" max="360" step="1" class="form-control input-sm" data-component="h"/>\
                            </div>\
                            <div class="color-control">\
                                <label class="control-label">S</label>\
                                <input type="number" min="0" max="100" step="1" class="form-control input-sm" data-component="s"/>\
                            </div>\
                            <div class="color-control">\
                                <label class="control-label">V</label>\
                                <input type="number" min="0" max="100" step="1" class="form-control input-sm" data-component="v"/>\
                            </div>\
                        </div>\
                        <div class="color-control-group">\
                            <div class="color-control">\
                                <label class="control-label">A</label>\
                                <input type="number" min="0" max="1" step="0.01" class="form-control input-sm" data-component="a"/>\
                            </div>\
                        </div>\
                    </div>\
                    <div class="color-setter-footer">\
                        <button class="color-setter-advanced btn btn-default btn-sm" type="button">\
                            <i class="glyphicon glyphicon-cog"></i>\
                            Advanced\
                        </button>\
                        <button class="color-setter-cancel btn btn-default btn-sm" type="button">\
                            <i class="glyphicon glyphicon-remove"></i>\
                            Cancel\
                        </button>\
                        <button class="color-setter-ok btn btn-primary btn-sm pull-right" type="button">\
                            <i class="glyphicon glyphicon-ok"></i>\
                            Ok\
                        </button>\
                    </div>\
                </div>'
            }
        },
        buildData: function() {
            this.$color = new Color(this.options.color);
        }, 
        buildPicker: function() {
            this.$picker = $(this.options.templates.picker)
            this.$container.append(this.$picker)

            function updateSL(x, y) {
                this.updateHSV( 
                    undefined, 
                    x, 
                    y, 
                    undefined
                )
                this.updateControl(true, false, true)
            }

            this.$saturation = this.$container.find(".color-setter-saturation")
            this.$saturationHandle = this.$saturation.find(".color-setter-handle")
                .dragaround({
                    start: $.proxy(updateSL, this),
                    move:  $.proxy(updateSL, this),
                    end:   $.proxy(updateSL, this),
                    minY:  1,
                    maxY:  0,
                    valX:  this.$color.s,
                    valY:  this.$color.v
                });

            function updateH(x, y) {
                this.updateHSV(
                    y, 
                    undefined, 
                    undefined, 
                    undefined
                )
                this.updateControl(true, false, true)
            }

            this.$hue = this.$container.find(".color-setter-hue")
            this.$hueHandle = this.$hue.find(".color-setter-handle")
                .dragaround({
                    axis: "y",
                    start: $.proxy(updateH, this),
                    move:  $.proxy(updateH, this),
                    end:   $.proxy(updateH, this),
                    minY:  1,
                    maxY:  0,
                    valY:  this.$color.h
                });          


            function updateA(x, y) {
                this.updateHSV(
                    undefined, 
                    undefined, 
                    undefined, 
                    x
                )
                this.updateControl(true, false, true)
            }

            this.$alpha = this.$container.find(".color-setter-alpha")
            this.$alphaHandle = this.$alpha.find(".color-setter-handle")
                .dragaround({
                    axis: "x",
                    start: $.proxy(updateA, this),
                    move:  $.proxy(updateA, this),
                    end:   $.proxy(updateA, this),
                    minX:  1,
                    maxX:  0,
                    valX:  this.$color.a
                });          

            this.$current = this.$container.find(".color-setter-current")
                .on('click', $.proxy(function() {
                    this.$previous.trigger('click')
                }, this));

            this.$previous = this.$container.find(".color-setter-previous")
                .css("backgroundColor", this.$color.toString())
                .on('click', $.proxy(function() {
                    var current = new Color(this.$previous.css("backgroundColor"));
                    this.$previous.css({backgroundColor: this.$color.toString()});
                    this.$color = current;
                    this.updateControl(true, true, true)
                }, this));

            this.$ok = this.$container.find(".color-setter-ok")
                .on("click", $.proxy(function() {
                    if (this.options.ok) this.options.ok()
                }, this))

            this.$cancel = this.$container.find(".color-setter-cancel")
                .on("click", $.proxy(function() {
                    this.$color = new Color(this.$cancel.attr("data-color"));
                    this.updateControl(true, true, true);
                    if (this.options.cancel) this.options.cancel()
                }, this))


            this.$advancedContainer = this.$container.find(".color-setter-controls-advanced")
                .hide()

            this.$advanced = this.$container.find(".color-setter-advanced")  
                .on("click", $.proxy(function() {
                    if (this.$advancedContainer.is(":visible")) {
                        this.$advancedContainer.hide()
                        this.$advanced.removeClass("active")
                        //this.$original.focus()
                    } else {
                        this.$advancedContainer.show()
                        this.$advanced.addClass("active")
                        //this.$original.focus()
                        this.updateControl(false, false, true)
                    }
                }, this))


            var self = this;


            this.$advancedControl = this.$advancedContainer.find(":input")
                .on("keydown", function(event) {
                    if (event.keyCode == 13) {
                        $(this).trigger("change");
                        event.preventDefault();
                        event.stopPropagation();
                    }
                }) 
                .on("change", function(event) {
                    
                    var el = $(this);
                    var value = $.fn.dragaround.plugin.prototype.convertTo(
                        parseFloat(el.val()),
                        parseFloat(el.attr("min")),
                        parseFloat(el.attr("max"))
                        );
                    value = Math.max(0, Math.min(value, 1));

                    if (value || value == 0) {
                        var array = {};
                        array[el.attr("data-component")] = value;
                        self.updateWithArray(array);
                    };

                    self.updateControl(true, true, true);
                    event.preventDefault();
                    event.stopPropagation();
                    
                })  
        },
        updateWithArray: function(el) {
            if ("r" in el || "g" in el || "b" in el) return this.updateRGB(el.r, el.g, el.b, el.a)
            return this.updateHSV(el.h, el.s, el.v, el.a)
        },
        updateRGB: function(r, g, b, a) {
            var col = this.$color.toRGB();
            if (r !== undefined) col.r = r;
            if (g !== undefined) col.g = g;
            if (b !== undefined) col.b = b;
            if (a !== undefined) col.a = a;
            col = this.$color.RGBtoHSV(col.r, col.g, col.b, col.a);
            return this.updateHSV(col.h, col.s, col.v, col.a)
        },
        updateHSV: function(h, s, v, a) {
            if (h !== undefined) this.$color.value.h = h;
            if (s !== undefined) this.$color.value.s = s;
            if (v !== undefined) this.$color.value.v = v;
            if (a !== undefined) this.$color.value.a = a;
            this.$color.value.valid = true;
            this.$color.input       = null;
            this.$color.format      = "hsl";
        },
        updateControl: function(updaterelated, updatehandler, updateadvanced) {

            var rgba = this.$color;
            var hue  = new Color({
                h:this.$color.value.h, 
                s:1, 
                v:1, 
                a:1
            });
            var rgb  = new Color({
                h:this.$color.value.h, 
                s:this.$color.value.s, 
                v:this.$color.value.v, 
                a:1
            });

            rgba = rgba.toString()
            rgb  = rgb.toString()
            hue  = hue.toString()

            this.$saturation.css({backgroundColor: hue});
            this.$alpha.css({backgroundColor: rgb});
            
            this.$container.find(".update-current").css({backgroundColor: rgba});



            if (updaterelated && this.options.change) {
                this.options.change(this.$color)
            }

            if (updatehandler) {
                this.$saturationHandle.dragaround("setX", this.$color.value.s)
                this.$saturationHandle.dragaround("setY", this.$color.value.v)
                this.$saturationHandle.dragaround("updatePosition")

                this.$alphaHandle.dragaround("setX", this.$color.value.a)
                this.$alphaHandle.dragaround("updatePosition")

                this.$hueHandle.dragaround("setY", this.$color.value.h)
                this.$hueHandle.dragaround("updatePosition")
            }

            if (updateadvanced && this.$advancedContainer.is(":visible")) {
                var component = $.extend(
                    this.$color.toRGB(
                        this.$color.value.h, 
                        this.$color.value.s, 
                        this.$color.value.v, 
                        this.$color.value.a
                        ),
                    this.$color.value
                    );

                $.each(this.$advancedControl, function() {
                    var color = $(this);
                    var value = component[color.attr("data-component")] * color.attr("max");

                    value = $.fn.dragaround.plugin.prototype.convertToReverse(
                        component[color.attr("data-component")],
                        parseFloat(color.attr("min")),
                        parseFloat(color.attr("max"))
                        )

                    value = $.fn.dragaround.plugin.prototype.fixedTo(
                        value,
                        parseFloat(color.attr("step"))
                        )
                    
                    color.val(value)
                })
            }
        }
    };

    var COLOR_FIELD_SETTER = null;
    
    function ColorControl(original, options) {

        this.options   = $.extend({}, this.defaults, options);
        this.$original = $(original);

        this.buildContainer();
        this.buildInput();
        this.buildDropdown();
        this.buildAddon();
        this.buildModal();
        this.buildData();

        this.updateControl(false, true, true);

        this.$isMobile = true



    };

    ColorControl.prototype = {
        constructor: ColorControl,
        defaults: {
            mobileWidth: 600,
            templates: {
                container: '<div class="color-setter-control"/>',
                addon: '<div class="color-setter-addon" data-toggle="dropdown"><div class="color-setter-color update-current"/></div>',
                dropdown: '<div class="dropdown-menu"/>',
                modal: '<div class="modal fade">\
                    <div class="modal-dialog modal-sm">\
                        <div class="modal-content">\
                            <div class="modal-header">\
                                <strong>Pick a color</strong>\
                            </div>\
                            <div class="modal-body">\
                            </div>\
                        </div>\
                    </div>\
                    </div>'
            }
        },
        buildData: function() {
            this.$color = new Color(this.$original.val());
        }, 
        buildContainer: function() {
            this.$container = $(this.options.templates.container)
            this.$original.after(this.$container)
        },
        buildInput: function() {
            this.$original
                .attr("type", "input")
                .addClass("form-control")
                .on("keyup", $.proxy(function(event) {
                    if (/^(38|40)$/.test(event.keyCode)) {
                        // keyup keydown

                        if (event.keyCode == 38 && this.$container.hasClass("open")) {
                            this.$addon.trigger("click.bs.dropdown")
                        }

                        if (event.keyCode == 40 && ! this.$container.hasClass("open")) {
                            this.$addon.trigger("click.bs.dropdown")
                        }

                        event.preventDefault()
                        event.stopPropagation()
                        return
                    }

                    this.buildData()
                    this.updateControl(false)
                    this.buildSetter()
                    
                }, this))
                .on("click", $.proxy(function(event) {
                    if (this.$container.hasClass("open")) event.stopPropagation();
                }, this))                
            this.$container.append(this.$original)
        },
        buildAddon: function() {
            this.$addon = $(this.options.templates.addon)
                .off("keydown.bs.dropdown.data-api")
                .on("click", $.proxy(this.route, this));

            if (this.$original.attr("disabled")) {
                this.$addon.addClass("disabled")
                    .attr("disabled", "disabled")
            }

            this.$original.after(this.$addon)
        },
        route: function() {

            if (this.$original.attr("disabled")) {
                this.$addon.removeAttr("data-toggle")

            } else {

                this.$isMobile = this.options.mobileWidth && $(window).width() <= this.options.mobileWidth;

                if (this.$isMobile) {
                    this.buildSetter()
                    this.$modal.modal()
                    this.$addon.attr("data-toggle", "modal")
                } else {
                    this.$addon.attr("data-toggle", "dropdown")
                }
            }
        },
        buildModal: function() {
            this.$modal = $(this.options.templates.modal)
            this.$original.after(this.$modal)
        },
        buildDropdown: function() {
            this.$dropdown = $(this.options.templates.dropdown)
                .on("click", function(event) {
                    //this will prevent the dropdown to close on click
                    event.stopPropagation()
                })
            this.$original.after(this.$dropdown)

            this.$container
                .on('show.bs.dropdown', $.proxy(this.buildSetter, this));

        },
        buildSetter:function() {

            if (! COLOR_FIELD_SETTER) {
                COLOR_FIELD_SETTER = $("<div/>").colorsetter();
            }

            this.$setter = COLOR_FIELD_SETTER;
            this.$setterAPI = COLOR_FIELD_SETTER.data("colorsetter");
            this.$setterAPI.$color = this.$color;

            this.$setterAPI.$previous.css({backgroundColor: this.$color.toString()});
            this.$setterAPI.$cancel.attr("data-color", this.$original.val());

            this.$setterAPI.options.change = $.proxy(function(color) {
                this.$color = color;
                this.updateControl(true);
            }, this)

            this.$setterAPI.options.ok = $.proxy(function() {
                if (this.$modal.hasClass("in")) {
                    this.$modal.modal("hide")
                }
                if (this.$container.hasClass("open")) {
                    this.$addon.trigger("click.bs.dropdown")
                };
            }, this)

            this.$setterAPI.options.cancel = this.$setterAPI.options.ok
            this.$setterAPI.updateControl(false, true, true);

            if (! this.$isMobile) {
                // route the modal to dropdown on web
                this.$addon.attr("data-toggle", "dropdown")
                this.$dropdown.append(this.$setter)
            } else {
                this.$addon.attr("data-toggle", "modal")
                this.$modal.find(".modal-body").append(this.$setter)
            }
        },
        updateControl:function(updateinput) {

            var rgba = this.$color.toString();

            if (this.$color.value.valid) {
                this.$addon.addClass("success");
            } else {
                this.$addon.removeClass("success");
            }
            
            this.$addon.children().css({backgroundColor: rgba});
            
            if (updateinput &&   this.$color.input) this.$original.val(this.$color.input);
            if (updateinput && ! this.$color.input) this.$original.val(rgba);
            if (updateinput && ! this.$color.value.valid && ! this.$color.input) this.$original.val("");

        }
    };

    $.fn.registerPlugin(ColorSetter, "colorsetter");
    $.fn.registerPlugin(ColorControl,  "colorcontrol");

}(window.jQuery);


/**
 * file setter plugin by riccardod@wolfram.com
 */

!function($) {

    function FileSetter(original, options) {

        this.options   = $.extend({}, this.defaults, options);
        this.$original = $(original);

        this.buildContainer();
        this.buildAddon();
        this.buildInput();
        this.buildRemove();

        this.buildBrowse();
        this.buildCapture();

        this.refresh();
    };

    FileSetter.prototype = {
        constructor: FileSetter,
        defaults: {
            templates: {
                container: '<div class="input-group input-filesetter"/>',
                input:  '<input type="text" class="form-control disabled file-label" readonly disabled>',
                remove: '<div class="input-group-btn file-close">\
                            <div class="btn btn-default">\
                                &times;\
                            </div>\
                        </div>',
                addon: '<div class="input-group-btn input-group-file"/>',
                browse: '<div class="btn btn-file btn-default">\
                            <i class="glyphicon glyphicon-folder-open"></i>\
                            <span class="filesetter-text">Browse</span>\
                        </div>',
                camera: '<div class="btn btn-file btn-default btn-camera hidden-sm hidden-md hidden-lg">\
                            <i class="glyphicon glyphicon-camera"></i>\
                        </div>',
                microphone: '<div class="btn btn-file btn-default btn-microphone hidden-sm hidden-md hidden-lg">\
                            <i class="glyphicon glyphicon-volume-up"></i>\
                        </div>',
            },
            capture: undefined
        },
        buildContainer: function() {
            this.$container = $(this.options.templates.container)
            this.$original.after(this.$container)
        },
        buildAddon: function() {
            this.$addon = $(this.options.templates.addon)
            this.$container.append(this.$addon)
        },
        buildInput: function() {
            this.$input = $(this.options.templates.input)
            this.$container.append(this.$input)
        },
        buildRemove: function() {
            this.$remove = $(this.options.templates.remove)
                .on("click", $.proxy(function() {
                    this.$remove.detach();
                    this.$input.val("");
                    this.$original.val("");
                }, this))
        },
        buildBrowse: function() {
            this.$browse = $(this.options.templates.browse)
            if (this.$original.attr("data-label")) {
                this.$browse.find(".filesetter-text").html(this.$original.attr("data-label"))
            }
            this.$addon.append(this.$browse)
            this.$original
                .removeAttr("capture")
                .on("change", $.proxy(this.refresh, this))

            if (this.$original.attr("disabled")) {
                this.$browse.addClass("disabled")
                    .attr("disabled", "disabled")
            }

            this.$addon.append(this.$original)
        },
        buildCapture: function() {

            if (this.options.capture == undefined || this.options.capture == true) {
                if (this.$original.attr("data-field-name") == "Image") this.options.capture = "camera";
                if (this.$original.attr("data-field-name") == "Sound") this.options.capture = "microphone";
                if (this.$original.attr("capture"))                    this.$original.attr("capture");
            }

            if (this.options.capture) {
                this.$capture = $(this.options.templates[this.options.capture])
                    .on("click", $.proxy(function() {
                        this.$original
                            .attr("capture", this.options.capture)
                            .trigger("click")
                            .removeAttr("capture")
                    }, this))
                this.$addon.append(this.$capture)
            }
        },
        refresh: function() {
            var numFiles = this.$original.get(0).files ? this.$original.get(0).files.length : 1;
            if (numFiles == 1) {
                this.$container.append(this.$remove);
                this.$input.val(this.$original.val().replace(/\\/g, '/').replace(/.*\//, ''));
            } else if (numFiles > 1) {
                this.$container.append(this.$remove);
                this.$input.val(numFiles + ' files selected');
            } else {
                this.$remove.detach();
            }
        }
    };

    $.fn.registerPlugin(FileSetter, "filesetter");

}(window.jQuery);



/**
 * smart control plugin by riccardod@wolfram.com
 */

!function($) {

    function SmartControl(original, options) {

        this.options   = $.extend({}, this.defaults, options);
        this.$original = $(original);

        this.buildInput();
        this.buildGroup();
        this.buildAddon();
        
    };

    SmartControl.prototype = {
        constructor: SmartControl,
        defaults: {
            templates: {
                addon: '<div class="icon-wolfram smart-icon form-control-feedback"/>'
            },
            groupClasses: "has-feedback form-group-smart",
            inputClasses: "smart-input",
            popover: {
              trigger:"hover",
              placement:"bottom",
              html:true,
              template:'<div class="popover popover-smart-control"><div class="arrow"></div><div class="popover-inner"><h3 class="popover-title"></h3><div class="popover-content"><p></p></div></div></div>',
            }
        },
        buildInput: function() {
            this.$input = this.$original;

            if (this.options.inputClasses) this.$input.addClass(this.options.groupClasses);
        },
        buildGroup: function() {
            this.$group = this.$original.wrap("<div></div>").parent();

            if (this.options.groupClasses) this.$group.addClass(this.options.groupClasses);
        },
        buildAddon: function() {
            this.$addon = $(this.options.templates.addon)

            if (this.$addon.hasClass("smart-icon") && this.$addon.hasClass("form-control-feedback")) {
                if (this.$original.hasClass("input-sm")) {
                    this.$addon.addClass("form-control-feedback-sm");
                } else if (this.$original.hasClass("input-lg")) {
                    this.$addon.addClass("form-control-feedback-lg");
                } else {
                    this.$addon.addClass("form-control-feedback-md");
                }                
            };
            this.$group.append(this.$addon)

            this.$addon.popover($.extend(
                true, 
                this.options.popover, 
                {content: this.popoverContent()}
            ));
        },
        popoverContent: function() {

            var prompt   = this.$original.attr("data-field-prompt");
            var verbose  = this.$original.attr("data-field-verbose") || this.$original.attr("data-field-name");
            var examples = this.$original.attr("data-field-examples");

            if (  prompt   ) prompt    = prompt.split("||");
            if (! prompt   ) prompt    = {};
            if (! prompt[0]) prompt[0] = "Enter any " + verbose;
            if (! prompt[1]) prompt[1] = "(Or anything specifying one)";

            // making verbose name bold.

            prompt[0] = prompt[0].replace(new RegExp(verbose, "i"), function (arg, match) {
                return '<strong>' + arg + '</strong>';
            });

            var s = '<h5 class="page-header smart-control-header">Wolfram Smart Field</h5>';
                s+= '<p class="smart-control-description">';
                s+= prompt[0]
                s+= '<br/>';
                s+= '<small>'+prompt[1]+'</small>';
                s+= '</p>';

            if (examples) {
                examples = examples.split("||");
                s += '<div class="smart-control-example-title">Examples:</div>';
                s += '<div class="smart-control-example-list">';
                for (var i in examples) s += '<div class="smart-control-example">'+examples[i]+'</div>';
                s += '</div>';
            }

            return s;
        }
    };    

    $.fn.registerPlugin(SmartControl, "smartcontrol");

}(window.jQuery);



/**
 * disambiguation control plugin by riccardod@wolfram.com
 */

!function($) {

    function DisambiguationControl(original, options) {

        this.options   = $.extend({}, this.defaults, options);
        this.$original = $(original);

        this.buildCloser();
        this.buildContainer();
        this.buildSelect();
        this.buildInput();
        this.buildHelp();
        
    };

    DisambiguationControl.prototype = {
        constructor: DisambiguationControl,
        defaults: {
            templates: {
                container: '<div class="input-group smart-select-cont"/>',
                input:     '<input type="text" class="form-control"/>',
                closer:    '<div class="input-group-btn"><div class="btn btn-default">&times;</div></div>'
            }
        },
        buildContainer: function() {
            this.$container = $(this.options.templates.container);
            this.$original.wrapInner(this.$container)
        },
        buildSelect: function() {

            var self = this;

            this.$select = this.$original
                .find("button[data-field-type=Semantic],button[data-field-type=Computed]")
                .on("keydown.bs.dropdown.data-api", function(event) {     

                    var is_open = self.$selectContainer.hasClass("open");

                    if (/^(8)$/.test(event.keyCode)) {

                        //on delete delete this guy
                        self.hideSelect(event)

                    } else if (/^(37)$/.test(event.keyCode))  {

                        //on key left move to the prev control
                        $(this).closest(".select").prevAll(".select").find(".form-control").focus();
                        if(is_open) $(this).trigger("click.bs.dropdown");

                    } else if (/^(39)$/.test(event.keyCode))  {

                        //on key right move to the prev control
                        $(this).closest(".select").nextAll(".select").find(".form-control").focus();
                        if(is_open) $(this).trigger("click.bs.dropdown");

                    } else if (/^(38)$/.test(event.keyCode) && ! is_open)  {
                        //on key up with navigation close show the input
                        self.hideSelect(event)
                    }
                });

            this.$selectContainer = this.$original
                .find(".select");

            this.$hidden = this.$original.closest(".form-group").find(".select-hidden");
            
            //this will move the chevron inside
            this.$selectContainer.each(function() {
                var self = $(this);
                self.children("button")
                    .append(
                        self.children(".form-control-feedback")
                            .removeClass("form-control-feedback")
                        )
                });

            this.$original
                .find(".disambiguation-text")
                    .addClass("form-control readonly")
                    .attr("readonly", "readonly").each(function() {
                        if(! $(this).text().trim()) $(this).hide()
                        });

            //this.$container.append(this.$selectContainer)
            this.$original.prepend(this.$hidden)

        },
        buildInput: function() {
            this.$input = $(this.options.templates.input)
            this.$original.prepend(this.$input)
            
            //copying all attrs before doing a smart control
            for (i = 0; i < this.$select[0].attributes.length; i++) {
                var a = this.$select[0].attributes[i];
                if (! /^(value|name|id|type|data-toggle|data-trigger|style)$/.test(a.name)) {
                    this.$input.attr(a.name, a.value);
                }
            };
            this.$input
                .smartcontrol()
                .on("click focus", $.proxy(function(event) {
                    if (this.$select.is(":visible")) {
                        this.$select.first()[event.type]();
                        event.stopPropagation();
                        event.preventDefault();
                    }
                }, this))
                .on("keydown", $.proxy(function(event) {
                    //navigation down|up
                    if (/^(38|40)$/.test(event.keyCode) && ! this.$select.is(":visible")) {
                        //navigation down
                        if (/^(40)$/.test(event.keyCode)) this.restoreSelect(event);
                        event.preventDefault();
                        event.stopPropagation();
                    }
                }, this));

        },
        buildCloser: function() {
            this.$closer = $(this.options.templates.closer)
                .on("click", $.proxy(this.hideSelect, this));
            this.$original.prepend(this.$closer)

        },
        buildHelp: function() {


            this.$help = this.$original.closest(".form-group").find(".help-error");


            if (/^(Review the choice. You typed )/.test(this.$help.text())) {
              var input_setter = $('<a class="help-input" href="#"></a>')
                .html(this.$original.attr("data-field-input"))
                .on('click', $.proxy(this.hideSelect, this));

              this.$help.html('Review the choice. ')
                .append($('<span class="glyphicon glyphicon-question-sign help-message help-icon"></span> ').popover({
                  content: "Submit the form to use this choice",
                  trigger: 'hover',
                  placement: 'right'
                }))
                .append(' <span class="help-message">You typed</span> ')
                .append(input_setter)
            }

            this.$didYouMean = $('<p class="help-block help-error"><span class="help-hint">Did you mean:</span> <a class="help-input" href="#"></a></p>');
              
            this.$help.after(this.$didYouMean);    

            this.$didYouMean
                .hide()
                .on('click', $.proxy(this.restoreSelect, this))


        },
        restoreSelect: function(event) {

            this.$input.removeAttr("name").val("");
            this.$hidden.removeAttr("disabled");
            this.$original.find(".smart-select-cont").show();
            this.$select.first().focus();
            this.$didYouMean.hide();
            this.$help.show();

            if (event) event.stopPropagation();
            if (event) event.preventDefault();
        },
        hideSelect: function(event) {


            this.$hidden.attr("disabled", "disabled");

            if (this.$selectContainer.hasClass("open")) this.$select.trigger("click.bs.dropdown");

            this.$original.find(".smart-select-cont").hide();

            this.$help.hide();
            this.$didYouMean
                .show()
                .find(".help-input")
                    .html(this.$original.find(".select-text,.disambiguation-text").text());

            this.$input
                .attr("name", this.$hidden.attr("name"))
                .val(this.$original.attr("data-field-input"))
                .focus();

            if (event) event.stopPropagation();
            if (event) event.preventDefault();
        }
    };

    $.fn.registerPlugin(DisambiguationControl, "disambiguationcontrol");

}(window.jQuery);


$(document).ready(function() {
  $('[type="range"]').slidercontrol()
});


$(document).ready(function() {
  $('select:not([multiple])').smartselect()
});


$(document).ready(function() {
  $('[type="color"]').colorcontrol()
});


$(document).ready(function() {
  $(":file:not(.image-capture)")
    .filesetter()


  $(document)
    .on('dragover', function (evt) {
      // Prevent the default browser drop action:
      $("body").addClass("body-state-drag");  
    })
    .on('dragleave drop dragend', function (evt) {
      $("body").removeClass("body-state-drag");
    })
});



$(document).ready(function() {
  $("input[data-field-type=Semantic],input[data-field-type=Computed]").smartcontrol();
});


$(document).ready(function() {
  $(".disambiguation-control").disambiguationcontrol();
});

/* Fixing tab index. this should be done after every widget */

$(document).ready(function() {
  $(":input:visible").each(function(index) {
    var self = $(this);
    if (self.attr("type") !== "hidden") {
      if (index == 0) {
        self.focus()
      };
      self.attr("tabindex", index + 1);
    };  
  });
});

/* injecting popover and tooltips */

$(document).ready(function() {
  $('[data-toggle="popover"]').popover({html:true});
  $('[data-toggle="tooltip"]').tooltip();
});

/* injecting popover and tooltips */

$(document).ready(function() {
  $(".col-delete .form-button").hover(
  function() {
    $(this)
      .addClass("form-button")
      .closest(".paneled-multicontrol")
      .addClass("paneled-multicontrol-remove")
  }, function() {
    $(this)
      .closest(".paneled-multicontrol")
      .removeClass("paneled-multicontrol-remove")
  });
});

/* injecting branding strip */

$(document).ready(function() {

    var link = $('<div class="wolfram-branding dropup">\
            <a class="wolfram-branding-cloud" href="http://www.wolfram.com/cloud/" target="_blank" data-toggle="dropdown" data-></a>\
            <ul class="wolfram-branding-menu dropdown-menu">\
                <div class="wolfram-branding-menu-body">\
                <!-- Dropdown menu links -->\
                <a class="wolfram-branding-link-primary" target="_blank" href="http://www.wolfram.com/cloud/">About Wolfram Cloud &raquo;</a>\
                <a class="wolfram-branding-link-secondary" target="_blank" href="http://www.wolfram.com/legal/terms/wolfram-cloud.html">Terms of use &raquo;</a>\
                <a class="wolfram-branding-link-secondary" target="_blank" href="http://www.wolfram.com/knowledgebase/source-information/">Data source information &raquo;</a>\
                </div>\
            </ul>\
        </div>');

    $("body")
        .append(link)
        .addClass("has-wolfram-branding");

    link
        .mouseleave(function(event) {
            if (link.hasClass("open")) link.find(".wolfram-branding-cloud").dropdown('toggle')
        })
        .find(".wolfram-branding-cloud")
        .hover(function(event) {
            if (! link.hasClass("open")) $(this).dropdown('toggle')
        })


});

/* injecting submit logic */

$(document).ready(function() {

    $( window ).unload(function() {
        // this is a safari fix for browser back
        $(".btn-submit-loading").button("reset")
    });

    var submitWithAddRemove = false;

    $('form .form-button').on("click", function(e) {
        submitWithAddRemove = true
        $(this)
            .find(".icon-default")
            .hide()
            .after('<i class="glyphicon glyphicon-refresh glyphicon-spin icon-loading"/>')

    })

    $('form').on("submit", function(e) {

        if ($(this).find("input[type=file]").length > 0) {
            $(this).attr("enctype", "multipart/form-data")

        } else {
            $(this).removeAttr("enctype")
        }

        var width  = $("[data-image-formatting-width]").width();
        var form   = $(this);
        var submit = form.find("[type=submit]")
        var button = submit.filter(".form-submit");

        if (width) {
            var input = form.find("[name=_imageformattingwidth]")
            if (input.length == 0) {
                input = $('<input type="hidden" name="_imageformattingwidth"/>');
                form.append(input);
            }
            input.val(width)
        }

        if (! submitWithAddRemove) {
            button
                .attr("autocomplete", "off")
                .addClass("btn-submit-loading")
                .attr("data-loading-text", 
                    button.attr("data-loading-text") || 
                    '<div class="loading-status">\
                        <i class="glyphicon glyphicon-stop"/>\
                        <i class="glyphicon glyphicon-stop"/>\
                        <i class="glyphicon glyphicon-stop"/>\
                        <i class="glyphicon glyphicon-stop"/>\
                    </div>'
                )
                .button("loading")   
                .addClass("btn-loading")  

        }

        /* perform processing then reset button when done */
        setTimeout(function() {
            submit.attr("disabled", "disabled")
        }, 100);

        /* perform processing then reset button when done */
        setTimeout(function() {

            submit
                .removeAttr("disabled")
                .filter(".btn-loading")
                .button('reset')

            form.find(".icon-loading").remove()
            form.find(".icon-default").show()
                
        }, 20000);
        
        //e.preventDefault();
        //e.stopPropagation()
    })



});

/* sending messages to the parent window  */

$(function() {
   
    function sendSize() {        
        //send it to the parent
        var data = {
            message: "dimensions", 
            width:  $("body").width(),
            height: $("body").height(),
            fields: $.map($("[data-field-name]"), function(i) {
                return $(i).attr("data-field-type") + $(i).attr("data-field-name")
            })
        };
        //console.log(data)
        parent.postMessage(data, "*");
    }

    $(window).on("resize", sendSize);
    $(document)
        .load(sendSize)
        .ready(function() {
            sendSize();
            setTimeout(sendSize,  500);
            setTimeout(sendSize, 1000);
            setTimeout(sendSize, 1500);
            setTimeout(sendSize, 2000);
        })

    /* 
    $(document)
        .on('show.bs.modal',      sendSize)
        .on('hidden.bs.modal',    sendSize)
        .on('show.bs.dropdown',   sendSize)
        .on('hidden.bs.dropdown', sendSize)
        .on('show.bs.tooltip',    sendSize)
        .on('hidden.bs.tooltip',  sendSize)
        .on('show.bs.popover',    sendSize)
        .on('hidden.bs.popover',  sendSize)
    */

});