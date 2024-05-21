# Wasm GC: Exploration Notes

## Boxing Primitive Types

```wat
(type $Boxed$i32 (struct (field $value i32)))

(func $box$i32 (param i32) (result (ref null any))
  (return (struct.new $Boxed$i32 (local.get 0)))
)

(func $unbox$i32 (param $x (ref null any)) (result i32)
  (local $boxed (ref $Boxed$i32))
  (local.set $boxed
    (ref.cast
      (ref $Boxed$i32)
      (local.get $x)
    )
  )
  (return
    (struct.get
      $Boxed$i32
      $value
      $boxed
    )
  )
)
```

## Questions

* How to box/unbox to get decent interop with JS?
* How do closures work? How much overhead do they require?
* How can prototype chains be expressed?
* What would structural typing look like?
* Best way of modelling index signatures?
* Where's the line between "self-contained" and "don't ship yet another basic math function"?

## References

* [wat2wasm demo](https://jkrems.dev/wabt/demo/wat2wasm/index.html)
* https://v8.dev/blog/wasm-gc-porting
