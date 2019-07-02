package Tests;

class A{
	private A(int i){

	}

	A(){

	}
}

class B{
	A x = new A();
	B(){
		A y = new A();
	}
}