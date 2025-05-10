let code = create<1024>() ;;

let load_code() =
  set(code,0,push);
  set(code,1,cur 2);
  set(code,2,push);
  set(code,3,push);
  set(code,4,cdr);
  set(code,5,swap);
  set(code,6,quote_int 0);
  set(code,7,cons);
  set(code,8,gt);
  set(code,9,branch 10 11);
  set(code,10,quote_bool true);
  set(code,11,quote_bool false);
  set(code,12,cons);
  set(code,13,push);
  set(code,14,cdr);
  set(code,15,swap);
  set(code,16,quote_int -5);
  set(code,17,cons);
  set(code,18,app);
;;
