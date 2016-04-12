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
	myinit(): C{
		self
	};
};
Class B {
	b_function() : B {
		self
	};

};
Class Main {
	main():C {
	{
	  (new C).myinit();
	  (new B).b_function();
	}	
	};
};
