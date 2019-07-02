package Tests.Failed;

abstract class A {

	A () {

	}

	abstract void a(int c);
	

}

abstract class B extends A{
    
}

final class C extends B{
	int a(int a){
		
	}
}

