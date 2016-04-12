class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

Class Main inherits C {
	main(): Object {
	  (new IO).out_int(a)
	};
};
