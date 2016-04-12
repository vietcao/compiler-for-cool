Class A{
	func() : Object {
		(new IO).out_string("Class A say hello\n")
	};
};
Class B inherits A{
	func() : Object {
		(new IO).out_string("Class B say hello\n")
	};
};

Class Main {
	id : A <- new A;
	main() : Object {
			
			let cd : A <- new B in{
					(new IO).out_string("Midle here\n");
					cd.func();
			}				
	};

};



