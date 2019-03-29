GridArrangement
~~~
add
~~~
arrange
~
// find optimum height, then map to range
return arrangeNR(container, g2, constraint);
~
// find optimum height
return arrangeRN(container, g2, constraint);
~
if (h == LengthConstraintType.FIXED) {
    // fixed width
    return arrangeRF(container, g2, constraint);
} else if (h == LengthConstraintType.RANGE) {
    return arrangeRR(container, g2, constraint);
}
~~~
arrangeNN
~~~
arrangeFF
~
List blocks = container.getBlocks();
~
for (int c = 0; c < this.columns; c++) {
    for (int r = 0; r < this.rows; r++) {
        int index = r * this.columns + c;
        if (index >= blocks.size()) {
            break;
        }
        Block b = (Block) blocks.get(index);
        if (b != null) {
            b.setBounds(new Rectangle2D.Double(c * width, r * height, width, height));
        }
    }
}
~~~
arrangeFR
~~~
arrangeRF
~~~
arrangeRN
~~~
arrangeNR
~~~
arrangeRR
~~~
arrangeFN
~~~
arrangeNF
~
break;
~
if (index >= blocks.size()) {
    break;
}
~~~
clear
~~~
equals
~
return true;
~
return false;
~
if (obj == this) {
    return true;
}
~
if (!(obj instanceof GridArrangement)) {
    return false;
}
