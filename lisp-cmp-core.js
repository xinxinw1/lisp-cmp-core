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
  var cmpsi = L.cmpsi;
  
  var jn = L.jn;
  var bol = L.bol;
  
  ////// JS functions //////
  
  jn({
  });
  
  bol({
  });
  
  ////// Import lisp //////
  
  function cmpf(a){
    return cmps($.get(a));
  }
  
  function cmpfi(a){
    return cmpsi($.get(a));
  }
  
  cmpf("lib/lisp-cmp-core.lisp");
  
  ////// Object exposure //////
  
  $.att({
    cmpf: cmpf,
    cmpfi: cmpfi
  }, L);
  
  ////// Testing //////
  
  
  
})(window);
