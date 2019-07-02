package Tests.Failed;

class A{
    A(){

    }
    int m;
    A(int m){
        this.m = m;
    }

    int getM(){
        return this.m;
    }
}

public class CallTest {
	public static void main(String[] args) {
        A x = new A(123);
        int result = x.getM();
        boolean b = x.getM();
	}
}
