package Tests.Passed;

abstract class A {

	A () {

	}

	public abstract void a(int c);
	

}

class B extends A{
    public void a(int c){

    }

    public int b(String s){
        return 0;
    }
}

final class C extends B{
    public int b(String x){
        return 0;
    }
}
 