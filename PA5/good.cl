Class C{
	var1 : Int <- 3 + 2;
};
Class A{
	var1 : Int;
	func() : Object {
		{
			var1 <- var1 + 1;
		}
	};

};
Class B inherits A{
	s : String <- "s_string";
	
	c : Int <- 2;
	a : A <- new B;
	b : B <- new B;
	get_y() : Object {
		1
	};
	get_s() : Object {
		self
	};
};

Class D{};
Class Main {
	main() : Object {
		self
	};

};



