package Tests.Failed;

class PrefixOpTest{
    PrefixOpTest(){
        int x = -5;
        float f  = (float) -5.5;
        boolean b = !false;
        float f1 = --f;
        // boolean wrong = !1.1;
        // boolean wrong = !'c';
        // boolean wrong = -'c';
        // boolean wrong = -b;
        int y = ~false;
        
    }
}