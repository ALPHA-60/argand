.IS

box nlbox {
        var nl1, nl2;
}

box null {
}

box line {
        var x,y;
        conn x to y;
}

box parent {
  var x;
  put child : null {
    var y;
    conn x to y;
  }
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

	put parent {
	  x = L1.y;
	  child.y = x + (0.1,1);
	  put null {
	    var xn,yn;
	    xn = x;
	    yn = xn + (2,0.1);
	    conn xn to yn;
	  }
	}

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
        
        put nlbox {
                nl1 = nl2 * nl2;
                nl2 = (2,1);
                conn nl1 to nl2;
        }

        put nlbox {
                nl1 = nl2 * nl2;
                nl2 = (3,1);
                conn nl1 to nl2;
        }

}

.IE
