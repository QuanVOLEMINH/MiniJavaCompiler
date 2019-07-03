package Tests.Passed;


abstract class A extends ClassInnerMemberModifierTest {

    A() {

    }

    abstract int a(String l[]);

}

public class ClassInnerMemberModifierTest {
    static int sum (int a, int b){
        return a + b;
    }

    int a (String l[]){
        return 0;
    }
    private static class C {
        public static class D{}
    }  

    private class F {
        void printSth(String a){
        }
    }

 	public static void main(String[] args) {
 	}

}