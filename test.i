.IS

box null {
}

box line {
        var x,y;
        conn x to y;
}

box main {
        var s, x, y, z;
        s = (3,1);
        x = 3;
        z = 2*cis(40);
        z = x + y;
        conn s to s + x;
        conn s to s + y;
        conn s to s + z;
        
        L1.x = s + y;
        L1.y = s + z;

        put L1 : line  {
                var w;
                w = 0.5 [x, y];
                conn w to z;
        }

        conn 0 to 360 using 7 line {
                var a1, a2, c, r;
                c = (4,4);
                r = 2;
                conn c + r*cis(a1) to c + r*cis(a2);

        } <a1, a2>;

}

.IE
