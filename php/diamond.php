<?php
interface IA { public function foo(); }
interface IB extends IA { public function foo(); }
interface IC extends IA { public function foo(); }

class D implements IB, IC {
    public function foo() { echo "D"; } // Must implement
}

$d = new D();
$d->foo(); // Output: D