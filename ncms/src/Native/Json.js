//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _aforemny$ncms$Native_Json = function() {


function expose(value) {

    var result = { ctor: "Null" };
    if (typeof value === "boolean") {
        result = { ctor: "Bool", _0: value };
    }
    if (typeof value === "number") {
        result = { ctor: "Number", _0: value };
    }
    if (typeof value === "string") {
        result = { ctor: "String", _0: value };
    }
    if (value != null) {
      if ((typeof value === "object")) {
          if (Array.isArray(value)) {
              var elements = _elm_lang$core$Native_List.Nil;
              for (var i = value.length - 1; i >= 0; i--) {
                  elements =
                      _elm_lang$core$Native_List.Cons(expose(value[i]), elements);
              }
              result = { ctor: "List", _0: elements };
          } else {
              var elements = _elm_lang$core$Native_List.Nil;
              for (var key in value) {
                  var tuple =
                      _elm_lang$core$Native_Utils.Tuple2(
                          key,
                          expose(value[key]),
                      );
                  elements =
                      _elm_lang$core$Native_List.Cons(tuple, elements);
              }
              result = { ctor: "Object", _0: _elm_lang$core$Dict$fromList(elements) };
          }
      }
    }
    return result;
}

return {
	expose: expose
};

}();
