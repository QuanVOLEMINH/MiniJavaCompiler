package Tests;

abstract class A {

	A () {

	}

	public abstract void a(int c);
	

}

class B extends A{
    void a(int c){

    }

    public int b(String s){}
}

final class C extends B{
    public int b(String x){
        return 0;
    }
}
 