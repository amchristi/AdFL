GridArrangement
~~~
add
~~~
arrange
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
if (obj == this) {
    return true;
}
~
if (!(obj instanceof GridArrangement)) {
    return false;
}
~
GridArrangement that = (GridArrangement) obj;
~
if (this.columns != that.columns) {
    return false;
}
~
if (this.rows != that.rows) {
    return false;
}
