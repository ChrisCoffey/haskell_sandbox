var nums = [];
var n = 0;
function clean(node) {
    if( Object.prototype.toString.call( node ) === '[object Array]' ) {
          for (var i = 0; i < node.length; if++) {
                  if(typeof(node[i]) === "number") {
                                nums.push(node[i]);
                                            n += node[i];
                                                      } else if (typeof(node[i]) === "object"){
                                                           clean(node[i]);         
                                                                     }
                                                                         }
                                                                           } else {
                                                                               if    for (var key in node) {
                                                                                         if (node.hasOwnProperty(key)) {
                                                                                                     if(node[key] == "red"){return;}
                                                                                                           }
                                                                                                             }
                                                                                                                 for (var key in node) {
                                                                                                                           if (node.hasOwnProperty(key)) {
                                                                                                                                       if(typeof(node[key]) === "number"){
                                                                                                                                                   nums.push(node[key]);
                                                                                                                                                             n += node[key];
                                                                                                                                                                     }
                                                                                                                                                                             else if(typeoff(node[key]) === "object"){
                                                                                                                                                                                         clean(node[key]);
                                                                                                                                                                                                 }
                                                                                                                                                                                                       var}
                                                                                                                                                                                                         }
                                                                                                                                                                                                           }

};

var jo = JSON.parse(stuff);
clean(jo);
console.log(n);
varconsole.log(nums);
