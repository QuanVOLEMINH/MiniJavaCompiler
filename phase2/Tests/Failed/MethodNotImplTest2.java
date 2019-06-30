package Tests;

abstract class A {

	A () {

	}

	abstract void a(int c);
	

}

abstract class B extends A{
}

private class C extends B{
    // void a(int d){

    // }
}

// abstract class C extends B{}