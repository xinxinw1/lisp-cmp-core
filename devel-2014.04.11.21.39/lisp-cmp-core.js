/***** Lisp Compiler Core Devel *****/

/* require tools >= 3.1 */
/* require ajax >= 4.1 */
/* require lisp-tools */
/* require lisp-parse */
/* require lisp-exec */
/* require lisp-compile */

(function (win, udf){
  ////// Import //////
  
  var cmps = L.cmps;
  
  var jn = L.jn;
  var bol = L.bol;
  
  ////// JS functions //////
  
  jn({
  });
  
  bol({
    "blk?": L.blkp,
    "ret?": L.retp,
    "thr?": L.thrp
  });
  
  ////// Import lisp //////
  
  function cmpf(a){
    return cmps($.get(a));
  }
  
  cmpf("/codes/libjs/lisp-cmp-core/devel/lisp-cmp-core.lisp");
  
  ////// Object exposure //////
  
  $.att({cmpf: cmpf}, L);
  
  ////// Testing //////
  
  
  
})(window);