let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,quote_null());
  set(code,2,cons);
  set(code,3,push);
  set(code,4,cur 5);
  set(code,5,push);
  set(code,6,push);
  set(code,7,cdr);
  set(code,8,swap);
  set(code,9,quote_int 0);
  set(code,10,cons);
  set(code,11,eq);
  set(code,12,branch 13 14);
  set(code,13,quote_int 0);
  set(code,14,push);
  set(code,15,car);
  set(code,16,cdr);
  set(code,17,swap);
  set(code,18,push);
  set(code,19,cdr);
  set(code,20,swap);
  set(code,21,quote_int 1);
  set(code,22,cons);
  set(code,23,sub);
  set(code,24,cons);
  set(code,25,app);
  set(code,26,swap);
  set(code,27,rplac);
  set(code,28,push);
  set(code,29,cdr);
  set(code,30,swap);
  set(code,31,quote_int 5);
  set(code,32,cons);
  set(code,33,app);
;;
