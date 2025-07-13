(module
 (type $0 (func (param i32 i32 i32) (result i32)))
 (type $1 (func (param i32)))
 (type $2 (func (param i32) (result i32)))
 (type $3 (func))
 (type $4 (func (param i32 i32 i32)))
 (type $5 (func (param i32 i32)))
 (type $6 (func (param i32 i64 i32) (result i64)))
 (type $7 (func (param i32 i32) (result i32)))
 (type $8 (func (param i32 i32 i32 i32) (result i32)))
 (type $9 (func (param i32 i32 i32 i32 i32) (result i32)))
 (type $10 (func (param i32 i32 i32 i32 i32)))
 (type $11 (func (result i32)))
 (import "wasi_snapshot_preview1" "proc_exit" (func $fimport$0 (param i32)))
 (import "wasi_snapshot_preview1" "fd_write" (func $fimport$1 (param i32 i32 i32 i32) (result i32)))
 (global $global$0 (mut i32) (i32.const 69312))
 (memory $0 258 258)
 (data $0 (i32.const 1024) "-+   0X0x\00\f0\9f\9a\80 Starting in-order traversal\00%d\00\f0\9f\a7\a8 Stack overflow! Expanding capacity...\00(null)\00(%d,%d)\00\n\e2\9c\85 In-order traversal complete!\00\f0\9f\8c\b1 Tree is empty. Nothing to traverse!\00\f0\9f\94\a2 Result: \00\n\f0\9f\93\8a STACK: \00, \00\f0\9f\93\a4 POP: Node %d, Moment %d\n\00\f0\9f\93\a5 PUSH: Node %d, Moment %d\n\00\f0\9f\8d\83 No right child for node %d\n\00\f0\9f\8d\83 No left child for node %d\n\00\f0\9f\8e\af VISIT: Node %d\n")
 (data $1 (i32.const 1392) "\19\00\0b\00\19\19\19\00\00\00\00\05\00\00\00\00\00\00\t\00\00\00\00\0b\00\00\00\00\00\00\00\00\19\00\n\n\19\19\19\03\n\07\00\01\00\t\0b\18\00\00\t\06\0b\00\00\0b\00\06\19\00\00\00\19\19\19")
 (data $2 (i32.const 1473) "\0e\00\00\00\00\00\00\00\00\19\00\0b\r\19\19\19\00\r\00\00\02\00\t\0e\00\00\00\t\00\0e\00\00\0e")
 (data $3 (i32.const 1531) "\0c")
 (data $4 (i32.const 1543) "\13\00\00\00\00\13\00\00\00\00\t\0c\00\00\00\00\00\0c\00\00\0c")
 (data $5 (i32.const 1589) "\10")
 (data $6 (i32.const 1601) "\0f\00\00\00\04\0f\00\00\00\00\t\10\00\00\00\00\00\10\00\00\10")
 (data $7 (i32.const 1647) "\12")
 (data $8 (i32.const 1659) "\11\00\00\00\00\11\00\00\00\00\t\12\00\00\00\00\00\12\00\00\12\00\00\1a\00\00\00\1a\1a\1a")
 (data $9 (i32.const 1714) "\1a\00\00\00\1a\1a\1a\00\00\00\00\00\00\t")
 (data $10 (i32.const 1763) "\14")
 (data $11 (i32.const 1775) "\17\00\00\00\00\17\00\00\00\00\t\14\00\00\00\00\00\14\00\00\14")
 (data $12 (i32.const 1821) "\16")
 (data $13 (i32.const 1833) "\15\00\00\00\00\15\00\00\00\00\t\16\00\00\00\00\00\16\00\00\16\00\000123456789ABCDEF")
 (data $14 (i32.const 1873) " \00\00\00\00\00\00\05")
 (data $15 (i32.const 1892) "\02")
 (data $16 (i32.const 1916) "\03\00\00\00\04\00\00\00\c8\08\00\00\00\04")
 (data $17 (i32.const 1940) "\01")
 (data $18 (i32.const 1956) "\ff\ff\ff\ff\n")
 (data $19 (i32.const 2024) "X\07\00\00\c0\0e\01")
 (table $0 5 5 funcref)
 (elem $0 (i32.const 1) $0 $3 $11 $12)
 (export "memory" (memory $0))
 (export "__indirect_function_table" (table $0))
 (export "_start" (func $1))
 (export "_emscripten_stack_restore" (func $24))
 (export "emscripten_stack_get_current" (func $25))
 (func $0
  (i32.store
   (i32.const 2184)
   (i32.const 2064)
  )
  (i32.store
   (i32.const 2144)
   (i32.const 65536)
  )
  (i32.store
   (i32.const 2140)
   (i32.const 69312)
  )
  (i32.store
   (i32.const 2112)
   (i32.const 42)
  )
  (i32.store
   (i32.const 2148)
   (i32.load
    (i32.const 1872)
   )
  )
 )
 (func $1
  (local $0 i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local $9 i32)
  (local $10 i32)
  (local $11 i64)
  (i32.store
   (i32.const 2184)
   (i32.const 2064)
  )
  (i32.store
   (i32.const 2144)
   (i32.const 65536)
  )
  (i32.store
   (i32.const 2140)
   (i32.const 69312)
  )
  (i32.store
   (i32.const 2112)
   (i32.const 42)
  )
  (i32.store
   (i32.const 2148)
   (i32.load
    (i32.const 1872)
   )
  )
  (i32.store
   (local.tee $5
    (call $19
     (i32.const 12)
    )
   )
   (i32.const 10)
  )
  (i64.store align=4
   (local.tee $0
    (call $19
     (i32.const 12)
    )
   )
   (i64.const 8)
  )
  (i32.store offset=4
   (local.get $5)
   (local.get $0)
  )
  (i32.store offset=8
   (local.tee $2
    (call $19
     (i32.const 12)
    )
   )
   (i32.const 0)
  )
  (i64.store align=4
   (local.get $2)
   (i64.const 12)
  )
  (i32.store offset=8
   (local.get $0)
   (local.get $2)
  )
  (i32.store offset=8
   (local.tee $0
    (call $19
     (i32.const 12)
    )
   )
   (i32.const 0)
  )
  (i64.store align=4
   (local.get $0)
   (i64.const 7)
  )
  (i32.store offset=8
   (local.get $5)
   (local.get $0)
  )
  (global.set $global$0
   (local.tee $1
    (i32.sub
     (global.get $global$0)
     (i32.const 176)
    )
   )
  )
  (block $block
   (if
    (i32.eqz
     (local.get $5)
    )
    (then
     (call $10
      (i32.const 1162)
     )
     (br $block)
    )
   )
   (i32.store offset=8
    (local.tee $4
     (call $19
      (i32.const 12)
     )
    )
    (i32.const 10)
   )
   (i32.store
    (local.get $4)
    (local.tee $3
     (call $19
      (i32.const 80)
     )
    )
   )
   (local.set $10
    (call $19
     (i32.const 400)
    )
   )
   (call $10
    (i32.const 1034)
   )
   (i32.store offset=4
    (local.get $4)
    (i32.const 0)
   )
   (i32.store offset=4
    (local.get $3)
    (i32.const 1)
   )
   (i32.store
    (local.get $3)
    (local.get $5)
   )
   (local.set $0
    (i32.load
     (local.get $5)
    )
   )
   (i32.store offset=164
    (local.get $1)
    (i32.const 1)
   )
   (i32.store offset=160
    (local.get $1)
    (local.get $0)
   )
   (call $2
    (i32.const 1264)
    (i32.add
     (local.get $1)
     (i32.const 160)
    )
   )
   (local.set $0
    (i32.const 0)
   )
   (loop $label1
    (call $2
     (i32.const 1217)
     (i32.const 0)
    )
    (block $block1
     (br_if $block1
      (i32.lt_s
       (local.tee $2
        (local.get $0)
       )
       (i32.const 0)
      )
     )
     (local.set $0
      (i32.load
       (i32.load
        (local.get $3)
       )
      )
     )
     (i32.store offset=148
      (local.get $1)
      (i32.load offset=4
       (local.get $3)
      )
     )
     (i32.store offset=144
      (local.get $1)
      (local.get $0)
     )
     (call $2
      (i32.const 1120)
      (i32.add
       (local.get $1)
       (i32.const 144)
      )
     )
     (local.set $0
      (i32.const 0)
     )
     (br_if $block1
      (i32.eqz
       (local.get $2)
      )
     )
     (loop $label
      (call $2
       (i32.const 1231)
       (i32.const 0)
      )
      (local.set $7
       (i32.load
        (i32.load
         (local.tee $6
          (i32.add
           (local.get $3)
           (i32.shl
            (local.tee $0
             (i32.add
              (local.get $0)
              (i32.const 1)
             )
            )
            (i32.const 3)
           )
          )
         )
        )
       )
      )
      (i32.store offset=132
       (local.get $1)
       (i32.load offset=4
        (local.get $6)
       )
      )
      (i32.store offset=128
       (local.get $1)
       (local.get $7)
      )
      (call $2
       (i32.const 1120)
       (i32.add
        (local.get $1)
        (i32.const 128)
       )
      )
      (br_if $label
       (i32.ne
        (local.get $0)
        (local.get $2)
       )
      )
     )
    )
    (call $7)
    (local.set $6
     (i32.load
      (local.tee $0
       (i32.add
        (local.get $3)
        (local.tee $7
         (i32.shl
          (local.get $2)
          (i32.const 3)
         )
        )
       )
      )
     )
    )
    (local.set $8
     (i32.load offset=4
      (local.get $0)
     )
    )
    (local.set $0
     (i32.load
      (i32.wrap_i64
       (local.tee $11
        (i64.load align=4
         (local.get $0)
        )
       )
      )
     )
    )
    (i64.store32 offset=116
     (local.get $1)
     (i64.shr_u
      (local.get $11)
      (i64.const 32)
     )
    )
    (i32.store offset=112
     (local.get $1)
     (local.get $0)
    )
    (call $2
     (i32.const 1234)
     (i32.add
      (local.get $1)
      (i32.const 112)
     )
    )
    (local.set $0
     (i32.sub
      (local.get $2)
      (i32.const 1)
     )
    )
    (block $block4
     (block $block3
      (block $block2
       (br_table $block2 $block3 $block4
        (i32.sub
         (local.get $8)
         (i32.const 1)
        )
       )
      )
      (if
       (i32.eq
        (i32.load offset=8
         (local.get $4)
        )
        (local.get $2)
       )
       (then
        (call $10
         (i32.const 1070)
        )
        (i32.store offset=8
         (local.get $4)
         (i32.shl
          (local.get $2)
          (i32.const 1)
         )
        )
        (i32.store
         (local.get $4)
         (local.tee $3
          (call $21
           (local.get $3)
           (i32.shl
            (local.get $2)
            (i32.const 4)
           )
          )
         )
        )
       )
      )
      (i32.store offset=4
       (local.tee $0
        (i32.add
         (local.get $3)
         (local.get $7)
        )
       )
       (i32.const 2)
      )
      (i32.store
       (local.get $0)
       (local.get $6)
      )
      (local.set $0
       (i32.load
        (local.get $6)
       )
      )
      (i32.store offset=52
       (local.get $1)
       (i32.const 2)
      )
      (i32.store offset=48
       (local.get $1)
       (local.get $0)
      )
      (call $2
       (i32.const 1264)
       (i32.add
        (local.get $1)
        (i32.const 48)
       )
      )
      (if
       (local.tee $7
        (i32.load offset=4
         (local.get $6)
        )
       )
       (then
        (if
         (i32.eq
          (i32.sub
           (local.tee $0
            (i32.load offset=8
             (local.get $4)
            )
           )
           (i32.const 1)
          )
          (local.get $2)
         )
         (then
          (call $10
           (i32.const 1070)
          )
          (i32.store offset=8
           (local.get $4)
           (i32.shl
            (local.get $0)
            (i32.const 1)
           )
          )
          (i32.store
           (local.get $4)
           (local.tee $3
            (call $21
             (local.get $3)
             (i32.shl
              (local.get $0)
              (i32.const 4)
             )
            )
           )
          )
         )
        )
        (i32.store offset=4
         (local.tee $2
          (i32.add
           (local.get $3)
           (i32.shl
            (local.tee $0
             (i32.add
              (local.get $2)
              (i32.const 1)
             )
            )
            (i32.const 3)
           )
          )
         )
         (i32.const 1)
        )
        (i32.store
         (local.get $2)
         (local.get $7)
        )
        (local.set $2
         (i32.load
          (local.get $7)
         )
        )
        (i32.store offset=36
         (local.get $1)
         (i32.const 1)
        )
        (i32.store offset=32
         (local.get $1)
         (local.get $2)
        )
        (call $2
         (i32.const 1264)
         (i32.add
          (local.get $1)
          (i32.const 32)
         )
        )
        (br $block4)
       )
      )
      (i32.store offset=16
       (local.get $1)
       (i32.load
        (local.get $6)
       )
      )
      (call $2
       (i32.const 1328)
       (i32.add
        (local.get $1)
        (i32.const 16)
       )
      )
      (local.set $0
       (local.get $2)
      )
      (br $block4)
     )
     (i32.store
      (i32.add
       (local.get $10)
       (i32.shl
        (local.get $9)
        (i32.const 2)
       )
      )
      (local.tee $8
       (i32.load
        (local.get $6)
       )
      )
     )
     (i32.store offset=96
      (local.get $1)
      (local.get $8)
     )
     (call $2
      (i32.const 1360)
      (i32.add
       (local.get $1)
       (i32.const 96)
      )
     )
     (local.set $9
      (i32.add
       (local.get $9)
       (i32.const 1)
      )
     )
     (if
      (local.tee $8
       (i32.load offset=8
        (local.get $6)
       )
      )
      (then
       (if
        (i32.eq
         (i32.load offset=8
          (local.get $4)
         )
         (local.get $2)
        )
        (then
         (call $10
          (i32.const 1070)
         )
         (i32.store offset=8
          (local.get $4)
          (i32.shl
           (local.get $2)
           (i32.const 1)
          )
         )
         (i32.store
          (local.get $4)
          (local.tee $3
           (call $21
            (local.get $3)
            (i32.shl
             (local.get $2)
             (i32.const 4)
            )
           )
          )
         )
        )
       )
       (i32.store offset=4
        (local.tee $0
         (i32.add
          (local.get $3)
          (local.get $7)
         )
        )
        (i32.const 1)
       )
       (i32.store
        (local.get $0)
        (local.get $8)
       )
       (local.set $0
        (i32.load
         (local.get $8)
        )
       )
       (i32.store offset=84
        (local.get $1)
        (i32.const 1)
       )
       (i32.store offset=80
        (local.get $1)
        (local.get $0)
       )
       (call $2
        (i32.const 1264)
        (i32.add
         (local.get $1)
         (i32.const 80)
        )
       )
       (local.set $0
        (local.get $2)
       )
       (br $block4)
      )
     )
     (i32.store offset=64
      (local.get $1)
      (i32.load
       (local.get $6)
      )
     )
     (call $2
      (i32.const 1295)
      (i32.sub
       (local.get $1)
       (i32.const -64)
      )
     )
    )
    (br_if $label1
     (i32.ne
      (local.get $0)
      (i32.const -1)
     )
    )
   )
   (i32.store offset=4
    (local.get $4)
    (i32.const -1)
   )
   (call $10
    (i32.const 1128)
   )
   (call $2
    (i32.const 1203)
    (i32.const 0)
   )
   (if
    (i32.gt_s
     (local.get $9)
     (i32.const 0)
    )
    (then
     (local.set $2
      (i32.sub
       (local.get $9)
       (i32.const 1)
      )
     )
     (local.set $0
      (i32.const 0)
     )
     (loop $label2
      (i32.store
       (local.get $1)
       (i32.load
        (i32.add
         (local.get $10)
         (i32.shl
          (local.get $0)
          (i32.const 2)
         )
        )
       )
      )
      (call $2
       (i32.const 1067)
       (local.get $1)
      )
      (if
       (i32.lt_s
        (local.get $0)
        (local.get $2)
       )
       (then
        (call $2
         (i32.const 1231)
         (i32.const 0)
        )
       )
      )
      (br_if $label2
       (i32.ne
        (local.tee $0
         (i32.add
          (local.get $0)
          (i32.const 1)
         )
        )
        (local.get $9)
       )
      )
     )
    )
   )
   (call $7)
   (call $20
    (local.get $3)
   )
   (call $20
    (local.get $4)
   )
   (call $20
    (local.get $10)
   )
  )
  (global.set $global$0
   (i32.add
    (local.get $1)
    (i32.const 176)
   )
  )
  (call $20
   (i32.load offset=8
    (i32.load offset=4
     (local.get $5)
    )
   )
  )
  (call $20
   (i32.load offset=4
    (local.get $5)
   )
  )
  (call $20
   (i32.load offset=8
    (local.get $5)
   )
  )
  (call $20
   (local.get $5)
  )
  (if
   (local.tee $0
    (i32.load
     (i32.const 2228)
    )
   )
   (then
    (loop $label3
     (call $4
      (local.get $0)
     )
     (br_if $label3
      (local.tee $0
       (i32.load offset=56
        (local.get $0)
       )
      )
     )
    )
   )
  )
  (call $4
   (i32.load
    (i32.const 2232)
   )
  )
  (call $4
   (i32.load
    (i32.const 2024)
   )
  )
  (call $4
   (i32.load
    (i32.const 2232)
   )
  )
  (call $fimport$0
   (i32.const 0)
  )
  (unreachable)
 )
 (func $2 (param $0 i32) (param $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (global.set $global$0
   (local.tee $4
    (i32.sub
     (global.get $global$0)
     (i32.const 16)
    )
   )
  )
  (i32.store offset=12
   (local.get $4)
   (local.get $1)
  )
  (global.set $global$0
   (local.tee $2
    (i32.sub
     (global.get $global$0)
     (i32.const 208)
    )
   )
  )
  (i32.store offset=204
   (local.get $2)
   (local.get $1)
  )
  (memory.fill
   (local.tee $1
    (i32.add
     (local.get $2)
     (i32.const 160)
    )
   )
   (i32.const 0)
   (i32.const 40)
  )
  (i32.store offset=200
   (local.get $2)
   (i32.load offset=204
    (local.get $2)
   )
  )
  (block $block
   (br_if $block
    (i32.lt_s
     (call $13
      (i32.const 0)
      (local.get $0)
      (i32.add
       (local.get $2)
       (i32.const 200)
      )
      (i32.add
       (local.get $2)
       (i32.const 80)
      )
      (local.get $1)
     )
     (i32.const 0)
    )
   )
   (local.set $1
    (i32.lt_s
     (i32.load
      (i32.const 1956)
     )
     (i32.const 0)
    )
   )
   (i32.store
    (i32.const 1880)
    (i32.and
     (local.tee $5
      (i32.load
       (i32.const 1880)
      )
     )
     (i32.const -33)
    )
   )
   (local.set $0
    (block $block3 (result i32)
     (block $block2
      (block $block1
       (if
        (i32.eqz
         (i32.load
          (i32.const 1928)
         )
        )
        (then
         (i32.store
          (i32.const 1928)
          (i32.const 80)
         )
         (i32.store
          (i32.const 1908)
          (i32.const 0)
         )
         (i64.store
          (i32.const 1896)
          (i64.const 0)
         )
         (local.set $3
          (i32.load
           (i32.const 1924)
          )
         )
         (i32.store
          (i32.const 1924)
          (local.get $2)
         )
         (br $block1)
        )
       )
       (br_if $block2
        (i32.load
         (i32.const 1896)
        )
       )
      )
      (drop
       (br_if $block3
        (i32.const -1)
        (call $5
         (i32.const 1880)
        )
       )
      )
     )
     (call $13
      (i32.const 1880)
      (local.get $0)
      (i32.add
       (local.get $2)
       (i32.const 200)
      )
      (i32.add
       (local.get $2)
       (i32.const 80)
      )
      (i32.add
       (local.get $2)
       (i32.const 160)
      )
     )
    )
   )
   (drop
    (if (result i32)
     (local.get $3)
     (then
      (drop
       (call_indirect (type $0)
        (i32.const 1880)
        (i32.const 0)
        (i32.const 0)
        (i32.load
         (i32.const 1916)
        )
       )
      )
      (i32.store
       (i32.const 1928)
       (i32.const 0)
      )
      (i32.store
       (i32.const 1924)
       (local.get $3)
      )
      (i32.store
       (i32.const 1908)
       (i32.const 0)
      )
      (drop
       (i32.load
        (i32.const 1900)
       )
      )
      (i64.store
       (i32.const 1896)
       (i64.const 0)
      )
      (i32.const 0)
     )
     (else
      (local.get $0)
     )
    )
   )
   (i32.store
    (i32.const 1880)
    (i32.or
     (i32.load
      (i32.const 1880)
     )
     (i32.and
      (local.get $5)
      (i32.const 32)
     )
    )
   )
   (br_if $block
    (local.get $1)
   )
  )
  (global.set $global$0
   (i32.add
    (local.get $2)
    (i32.const 208)
   )
  )
  (global.set $global$0
   (i32.add
    (local.get $4)
    (i32.const 16)
   )
  )
 )
 (func $3 (param $0 i32) (result i32)
  (i32.const 0)
 )
 (func $4 (param $0 i32)
  (local $1 i32)
  (local $2 i32)
  (block $block
   (br_if $block
    (i32.eqz
     (local.get $0)
    )
   )
   (drop
    (i32.load offset=76
     (local.get $0)
    )
   )
   (if
    (i32.ne
     (i32.load offset=20
      (local.get $0)
     )
     (i32.load offset=28
      (local.get $0)
     )
    )
    (then
     (drop
      (call_indirect (type $0)
       (local.get $0)
       (i32.const 0)
       (i32.const 0)
       (i32.load offset=36
        (local.get $0)
       )
      )
     )
    )
   )
   (br_if $block
    (i32.eq
     (local.tee $1
      (i32.load offset=4
       (local.get $0)
      )
     )
     (local.tee $2
      (i32.load offset=8
       (local.get $0)
      )
     )
    )
   )
   (drop
    (call_indirect (type $6)
     (local.get $0)
     (i64.extend_i32_s
      (i32.sub
       (local.get $1)
       (local.get $2)
      )
     )
     (i32.const 1)
     (i32.load offset=40
      (local.get $0)
     )
    )
   )
  )
 )
 (func $5 (param $0 i32) (result i32)
  (local $1 i32)
  (i32.store offset=72
   (local.get $0)
   (i32.or
    (i32.sub
     (local.tee $1
      (i32.load offset=72
       (local.get $0)
      )
     )
     (i32.const 1)
    )
    (local.get $1)
   )
  )
  (if
   (i32.and
    (local.tee $1
     (i32.load
      (local.get $0)
     )
    )
    (i32.const 8)
   )
   (then
    (i32.store
     (local.get $0)
     (i32.or
      (local.get $1)
      (i32.const 32)
     )
    )
    (return
     (i32.const -1)
    )
   )
  )
  (i64.store offset=4 align=4
   (local.get $0)
   (i64.const 0)
  )
  (i32.store offset=28
   (local.get $0)
   (local.tee $1
    (i32.load offset=44
     (local.get $0)
    )
   )
  )
  (i32.store offset=20
   (local.get $0)
   (local.get $1)
  )
  (i32.store offset=16
   (local.get $0)
   (i32.add
    (local.get $1)
    (i32.load offset=48
     (local.get $0)
    )
   )
  )
  (i32.const 0)
 )
 (func $6
  (local $0 i32)
  (local $1 i32)
  (global.set $global$0
   (local.tee $0
    (i32.sub
     (global.get $global$0)
     (i32.const 16)
    )
   )
  )
  (i32.store8 offset=15
   (local.get $0)
   (i32.const 10)
  )
  (block $block
   (block $block1
    (br_if $block1
     (i32.eq
      (if (result i32)
       (local.tee $1
        (i32.load
         (i32.const 1896)
        )
       )
       (then
        (local.get $1)
       )
       (else
        (br_if $block
         (call $5
          (i32.const 1880)
         )
        )
        (i32.load
         (i32.const 1896)
        )
       )
      )
      (local.tee $1
       (i32.load
        (i32.const 1900)
       )
      )
     )
    )
    (br_if $block1
     (i32.eq
      (i32.load
       (i32.const 1960)
      )
      (i32.const 10)
     )
    )
    (i32.store
     (i32.const 1900)
     (i32.add
      (local.get $1)
      (i32.const 1)
     )
    )
    (i32.store8
     (local.get $1)
     (i32.const 10)
    )
    (br $block)
   )
   (br_if $block
    (i32.ne
     (call_indirect (type $0)
      (i32.const 1880)
      (i32.add
       (local.get $0)
       (i32.const 15)
      )
      (i32.const 1)
      (i32.load
       (i32.const 1916)
      )
     )
     (i32.const 1)
    )
   )
   (drop
    (i32.load8_u offset=15
     (local.get $0)
    )
   )
  )
  (global.set $global$0
   (i32.add
    (local.get $0)
    (i32.const 16)
   )
  )
 )
 (func $7
  (local $0 i32)
  (block $block
   (if
    (i32.ge_s
     (local.tee $0
      (i32.load
       (i32.const 1956)
      )
     )
     (i32.const 0)
    )
    (then
     (br_if $block
      (i32.eqz
       (local.get $0)
      )
     )
     (br_if $block
      (i32.ne
       (i32.load
        (i32.const 2112)
       )
       (i32.and
        (local.get $0)
        (i32.const 1073741823)
       )
      )
     )
    )
   )
   (block $block1
    (br_if $block1
     (i32.eq
      (i32.load
       (i32.const 1960)
      )
      (i32.const 10)
     )
    )
    (br_if $block1
     (i32.eq
      (local.tee $0
       (i32.load
        (i32.const 1900)
       )
      )
      (i32.load
       (i32.const 1896)
      )
     )
    )
    (i32.store
     (i32.const 1900)
     (i32.add
      (local.get $0)
      (i32.const 1)
     )
    )
    (i32.store8
     (local.get $0)
     (i32.const 10)
    )
    (return)
   )
   (call $6)
   (return)
  )
  (i32.store
   (i32.const 1956)
   (select
    (local.tee $0
     (i32.load
      (i32.const 1956)
     )
    )
    (i32.const 1073741823)
    (local.get $0)
   )
  )
  (block $block3
   (block $block2
    (br_if $block2
     (i32.eq
      (i32.load
       (i32.const 1960)
      )
      (i32.const 10)
     )
    )
    (br_if $block2
     (i32.eq
      (local.tee $0
       (i32.load
        (i32.const 1900)
       )
      )
      (i32.load
       (i32.const 1896)
      )
     )
    )
    (i32.store
     (i32.const 1900)
     (i32.add
      (local.get $0)
      (i32.const 1)
     )
    )
    (i32.store8
     (local.get $0)
     (i32.const 10)
    )
    (br $block3)
   )
   (call $6)
  )
  (drop
   (i32.load
    (i32.const 1956)
   )
  )
  (i32.store
   (i32.const 1956)
   (i32.const 0)
  )
 )
 (func $8 (param $0 i32) (param $1 i32) (param $2 i32)
  (local $3 i32)
  (local $4 i32)
  (if
   (i32.ge_u
    (local.get $2)
    (i32.const 512)
   )
   (then
    (if
     (local.get $2)
     (then
      (memory.copy
       (local.get $0)
       (local.get $1)
       (local.get $2)
      )
     )
    )
    (return)
   )
  )
  (local.set $3
   (i32.add
    (local.get $0)
    (local.get $2)
   )
  )
  (block $block2
   (if
    (i32.eqz
     (i32.and
      (i32.xor
       (local.get $0)
       (local.get $1)
      )
      (i32.const 3)
     )
    )
    (then
     (block $block
      (if
       (i32.eqz
        (i32.and
         (local.get $0)
         (i32.const 3)
        )
       )
       (then
        (local.set $2
         (local.get $0)
        )
        (br $block)
       )
      )
      (if
       (i32.eqz
        (local.get $2)
       )
       (then
        (local.set $2
         (local.get $0)
        )
        (br $block)
       )
      )
      (local.set $2
       (local.get $0)
      )
      (loop $label
       (i32.store8
        (local.get $2)
        (i32.load8_u
         (local.get $1)
        )
       )
       (local.set $1
        (i32.add
         (local.get $1)
         (i32.const 1)
        )
       )
       (br_if $block
        (i32.eqz
         (i32.and
          (local.tee $2
           (i32.add
            (local.get $2)
            (i32.const 1)
           )
          )
          (i32.const 3)
         )
        )
       )
       (br_if $label
        (i32.lt_u
         (local.get $2)
         (local.get $3)
        )
       )
      )
     )
     (local.set $0
      (i32.and
       (local.get $3)
       (i32.const -4)
      )
     )
     (block $block1
      (br_if $block1
       (i32.lt_u
        (local.get $3)
        (i32.const 64)
       )
      )
      (br_if $block1
       (i32.gt_u
        (local.get $2)
        (local.tee $4
         (i32.add
          (local.get $0)
          (i32.const -64)
         )
        )
       )
      )
      (loop $label1
       (i32.store
        (local.get $2)
        (i32.load
         (local.get $1)
        )
       )
       (i32.store offset=4
        (local.get $2)
        (i32.load offset=4
         (local.get $1)
        )
       )
       (i32.store offset=8
        (local.get $2)
        (i32.load offset=8
         (local.get $1)
        )
       )
       (i32.store offset=12
        (local.get $2)
        (i32.load offset=12
         (local.get $1)
        )
       )
       (i32.store offset=16
        (local.get $2)
        (i32.load offset=16
         (local.get $1)
        )
       )
       (i32.store offset=20
        (local.get $2)
        (i32.load offset=20
         (local.get $1)
        )
       )
       (i32.store offset=24
        (local.get $2)
        (i32.load offset=24
         (local.get $1)
        )
       )
       (i32.store offset=28
        (local.get $2)
        (i32.load offset=28
         (local.get $1)
        )
       )
       (i32.store offset=32
        (local.get $2)
        (i32.load offset=32
         (local.get $1)
        )
       )
       (i32.store offset=36
        (local.get $2)
        (i32.load offset=36
         (local.get $1)
        )
       )
       (i32.store offset=40
        (local.get $2)
        (i32.load offset=40
         (local.get $1)
        )
       )
       (i32.store offset=44
        (local.get $2)
        (i32.load offset=44
         (local.get $1)
        )
       )
       (i32.store offset=48
        (local.get $2)
        (i32.load offset=48
         (local.get $1)
        )
       )
       (i32.store offset=52
        (local.get $2)
        (i32.load offset=52
         (local.get $1)
        )
       )
       (i32.store offset=56
        (local.get $2)
        (i32.load offset=56
         (local.get $1)
        )
       )
       (i32.store offset=60
        (local.get $2)
        (i32.load offset=60
         (local.get $1)
        )
       )
       (local.set $1
        (i32.sub
         (local.get $1)
         (i32.const -64)
        )
       )
       (br_if $label1
        (i32.le_u
         (local.tee $2
          (i32.sub
           (local.get $2)
           (i32.const -64)
          )
         )
         (local.get $4)
        )
       )
      )
     )
     (br_if $block2
      (i32.le_u
       (local.get $0)
       (local.get $2)
      )
     )
     (loop $label2
      (i32.store
       (local.get $2)
       (i32.load
        (local.get $1)
       )
      )
      (local.set $1
       (i32.add
        (local.get $1)
        (i32.const 4)
       )
      )
      (br_if $label2
       (i32.lt_u
        (local.tee $2
         (i32.add
          (local.get $2)
          (i32.const 4)
         )
        )
        (local.get $0)
       )
      )
     )
     (br $block2)
    )
   )
   (if
    (i32.lt_u
     (local.get $3)
     (i32.const 4)
    )
    (then
     (local.set $2
      (local.get $0)
     )
     (br $block2)
    )
   )
   (if
    (i32.lt_u
     (local.tee $4
      (i32.sub
       (local.get $3)
       (i32.const 4)
      )
     )
     (local.get $0)
    )
    (then
     (local.set $2
      (local.get $0)
     )
     (br $block2)
    )
   )
   (local.set $2
    (local.get $0)
   )
   (loop $label3
    (i32.store8
     (local.get $2)
     (i32.load8_u
      (local.get $1)
     )
    )
    (i32.store8 offset=1
     (local.get $2)
     (i32.load8_u offset=1
      (local.get $1)
     )
    )
    (i32.store8 offset=2
     (local.get $2)
     (i32.load8_u offset=2
      (local.get $1)
     )
    )
    (i32.store8 offset=3
     (local.get $2)
     (i32.load8_u offset=3
      (local.get $1)
     )
    )
    (local.set $1
     (i32.add
      (local.get $1)
      (i32.const 4)
     )
    )
    (br_if $label3
     (i32.le_u
      (local.tee $2
       (i32.add
        (local.get $2)
        (i32.const 4)
       )
      )
      (local.get $4)
     )
    )
   )
  )
  (if
   (i32.lt_u
    (local.get $2)
    (local.get $3)
   )
   (then
    (loop $label4
     (i32.store8
      (local.get $2)
      (i32.load8_u
       (local.get $1)
      )
     )
     (local.set $1
      (i32.add
       (local.get $1)
       (i32.const 1)
      )
     )
     (br_if $label4
      (i32.ne
       (local.tee $2
        (i32.add
         (local.get $2)
         (i32.const 1)
        )
       )
       (local.get $3)
      )
     )
    )
   )
  )
 )
 (func $9 (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (block $block
   (if
    (i32.lt_u
     (i32.sub
      (if (result i32)
       (local.tee $3
        (i32.load offset=16
         (local.get $2)
        )
       )
       (then
        (local.get $3)
       )
       (else
        (br_if $block
         (call $5
          (local.get $2)
         )
        )
        (i32.load offset=16
         (local.get $2)
        )
       )
      )
      (local.tee $4
       (i32.load offset=20
        (local.get $2)
       )
      )
     )
     (local.get $1)
    )
    (then
     (return
      (call_indirect (type $0)
       (local.get $2)
       (local.get $0)
       (local.get $1)
       (i32.load offset=36
        (local.get $2)
       )
      )
     )
    )
   )
   (block $block2
    (block $block1
     (br_if $block1
      (i32.lt_s
       (i32.load offset=80
        (local.get $2)
       )
       (i32.const 0)
      )
     )
     (br_if $block1
      (i32.eqz
       (local.get $1)
      )
     )
     (local.set $3
      (local.get $1)
     )
     (loop $label
      (if
       (i32.ne
        (i32.load8_u
         (i32.sub
          (local.tee $5
           (i32.add
            (local.get $0)
            (local.get $3)
           )
          )
          (i32.const 1)
         )
        )
        (i32.const 10)
       )
       (then
        (br_if $label
         (local.tee $3
          (i32.sub
           (local.get $3)
           (i32.const 1)
          )
         )
        )
        (br $block1)
       )
      )
     )
     (br_if $block
      (i32.lt_u
       (local.tee $4
        (call_indirect (type $0)
         (local.get $2)
         (local.get $0)
         (local.get $3)
         (i32.load offset=36
          (local.get $2)
         )
        )
       )
       (local.get $3)
      )
     )
     (local.set $1
      (i32.sub
       (local.get $1)
       (local.get $3)
      )
     )
     (local.set $4
      (i32.load offset=20
       (local.get $2)
      )
     )
     (br $block2)
    )
    (local.set $5
     (local.get $0)
    )
    (local.set $3
     (i32.const 0)
    )
   )
   (call $8
    (local.get $4)
    (local.get $5)
    (local.get $1)
   )
   (i32.store offset=20
    (local.get $2)
    (i32.add
     (i32.load offset=20
      (local.get $2)
     )
     (local.get $1)
    )
   )
   (local.set $4
    (i32.add
     (local.get $1)
     (local.get $3)
    )
   )
  )
  (local.get $4)
 )
 (func $10 (param $0 i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (drop
   (i32.load
    (i32.const 1956)
   )
  )
  (block $block5
   (br_if $block5
    (i32.ne
     (block $block4 (result i32)
      (drop
       (br_if $block4
        (local.tee $0
         (block $block1 (result i32)
          (block $block2
           (block $block
            (br_if $block
             (i32.eqz
              (i32.and
               (local.tee $2
                (local.get $0)
               )
               (i32.const 3)
              )
             )
            )
            (drop
             (br_if $block1
              (i32.const 0)
              (i32.eqz
               (i32.load8_u
                (local.get $0)
               )
              )
             )
            )
            (loop $label
             (br_if $block
              (i32.eqz
               (i32.and
                (local.tee $0
                 (i32.add
                  (local.get $0)
                  (i32.const 1)
                 )
                )
                (i32.const 3)
               )
              )
             )
             (br_if $label
              (i32.load8_u
               (local.get $0)
              )
             )
            )
            (br $block2)
           )
           (loop $label1
            (local.set $0
             (i32.add
              (local.tee $1
               (local.get $0)
              )
              (i32.const 4)
             )
            )
            (br_if $label1
             (i32.eq
              (i32.and
               (i32.or
                (i32.sub
                 (i32.const 16843008)
                 (local.tee $3
                  (i32.load
                   (local.get $1)
                  )
                 )
                )
                (local.get $3)
               )
               (i32.const -2139062144)
              )
              (i32.const -2139062144)
             )
            )
           )
           (loop $label2
            (local.set $1
             (i32.add
              (local.tee $0
               (local.get $1)
              )
              (i32.const 1)
             )
            )
            (br_if $label2
             (i32.load8_u
              (local.get $0)
             )
            )
           )
          )
          (i32.sub
           (local.get $0)
           (local.get $2)
          )
         )
        )
        (i32.eq
         (local.get $0)
         (local.tee $1
          (block $block3 (result i32)
           (if
            (i32.lt_s
             (i32.load
              (i32.const 1956)
             )
             (i32.const 0)
            )
            (then
             (br $block3
              (call $9
               (local.get $2)
               (local.get $0)
               (i32.const 1880)
              )
             )
            )
           )
           (call $9
            (local.get $2)
            (local.get $0)
            (i32.const 1880)
           )
          )
         )
        )
       )
      )
      (local.get $1)
     )
     (local.get $0)
    )
   )
   (block $block6
    (br_if $block6
     (i32.eq
      (i32.load
       (i32.const 1960)
      )
      (i32.const 10)
     )
    )
    (br_if $block6
     (i32.eq
      (local.tee $0
       (i32.load
        (i32.const 1900)
       )
      )
      (i32.load
       (i32.const 1896)
      )
     )
    )
    (i32.store
     (i32.const 1900)
     (i32.add
      (local.get $0)
      (i32.const 1)
     )
    )
    (i32.store8
     (local.get $0)
     (i32.const 10)
    )
    (br $block5)
   )
   (call $6)
  )
 )
 (func $11 (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local $9 i32)
  (global.set $global$0
   (local.tee $3
    (i32.sub
     (global.get $global$0)
     (i32.const 32)
    )
   )
  )
  (i32.store offset=16
   (local.get $3)
   (local.tee $4
    (i32.load offset=28
     (local.get $0)
    )
   )
  )
  (local.set $5
   (i32.load offset=20
    (local.get $0)
   )
  )
  (i32.store offset=28
   (local.get $3)
   (local.get $2)
  )
  (i32.store offset=24
   (local.get $3)
   (local.get $1)
  )
  (i32.store offset=20
   (local.get $3)
   (local.tee $1
    (i32.sub
     (local.get $5)
     (local.get $4)
    )
   )
  )
  (local.set $5
   (i32.add
    (local.get $1)
    (local.get $2)
   )
  )
  (local.set $7
   (i32.const 2)
  )
  (local.set $0
   (block $block3 (result i32)
    (block $block2
     (block $block1
      (block $block
       (if
        (if (result i32)
         (local.tee $4
          (call $fimport$1
           (i32.load offset=60
            (local.get $0)
           )
           (local.tee $1
            (i32.add
             (local.get $3)
             (i32.const 16)
            )
           )
           (i32.const 2)
           (i32.add
            (local.get $3)
            (i32.const 12)
           )
          )
         )
         (then
          (i32.store
           (i32.const 2220)
           (local.get $4)
          )
          (i32.const -1)
         )
         (else
          (i32.const 0)
         )
        )
        (then
         (local.set $4
          (local.get $1)
         )
         (br $block)
        )
       )
       (loop $label
        (br_if $block1
         (i32.eq
          (local.get $5)
          (local.tee $6
           (i32.load offset=12
            (local.get $3)
           )
          )
         )
        )
        (if
         (i32.lt_s
          (local.get $6)
          (i32.const 0)
         )
         (then
          (local.set $4
           (local.get $1)
          )
          (br $block2)
         )
        )
        (i32.store
         (local.tee $4
          (i32.add
           (local.get $1)
           (select
            (i32.const 8)
            (i32.const 0)
            (local.tee $9
             (i32.gt_u
              (local.get $6)
              (local.tee $8
               (i32.load offset=4
                (local.get $1)
               )
              )
             )
            )
           )
          )
         )
         (i32.add
          (local.tee $8
           (i32.sub
            (local.get $6)
            (select
             (local.get $8)
             (i32.const 0)
             (local.get $9)
            )
           )
          )
          (i32.load
           (local.get $4)
          )
         )
        )
        (i32.store
         (local.tee $1
          (i32.add
           (local.get $1)
           (select
            (i32.const 12)
            (i32.const 4)
            (local.get $9)
           )
          )
         )
         (i32.sub
          (i32.load
           (local.get $1)
          )
          (local.get $8)
         )
        )
        (local.set $5
         (i32.sub
          (local.get $5)
          (local.get $6)
         )
        )
        (br_if $label
         (i32.eqz
          (if (result i32)
           (local.tee $6
            (call $fimport$1
             (i32.load offset=60
              (local.get $0)
             )
             (local.tee $1
              (local.get $4)
             )
             (local.tee $7
              (i32.sub
               (local.get $7)
               (local.get $9)
              )
             )
             (i32.add
              (local.get $3)
              (i32.const 12)
             )
            )
           )
           (then
            (i32.store
             (i32.const 2220)
             (local.get $6)
            )
            (i32.const -1)
           )
           (else
            (i32.const 0)
           )
          )
         )
        )
       )
      )
      (br_if $block2
       (i32.ne
        (local.get $5)
        (i32.const -1)
       )
      )
     )
     (i32.store offset=28
      (local.get $0)
      (local.tee $1
       (i32.load offset=44
        (local.get $0)
       )
      )
     )
     (i32.store offset=20
      (local.get $0)
      (local.get $1)
     )
     (i32.store offset=16
      (local.get $0)
      (i32.add
       (local.get $1)
       (i32.load offset=48
        (local.get $0)
       )
      )
     )
     (br $block3
      (local.get $2)
     )
    )
    (i32.store offset=28
     (local.get $0)
     (i32.const 0)
    )
    (i64.store offset=16
     (local.get $0)
     (i64.const 0)
    )
    (i32.store
     (local.get $0)
     (i32.or
      (i32.load
       (local.get $0)
      )
      (i32.const 32)
     )
    )
    (drop
     (br_if $block3
      (i32.const 0)
      (i32.eq
       (local.get $7)
       (i32.const 2)
      )
     )
    )
    (i32.sub
     (local.get $2)
     (i32.load offset=4
      (local.get $4)
     )
    )
   )
  )
  (global.set $global$0
   (i32.add
    (local.get $3)
    (i32.const 32)
   )
  )
  (local.get $0)
 )
 (func $12 (param $0 i32) (param $1 i64) (param $2 i32) (result i64)
  (i64.const 0)
 )
 (func $13 (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32) (result i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local $9 i32)
  (local $10 i32)
  (local $11 i32)
  (local $12 i32)
  (local $13 i32)
  (local $14 i32)
  (local $15 i32)
  (local $16 i32)
  (local $17 i32)
  (local $18 i32)
  (local $19 i32)
  (local $20 i32)
  (local $21 i32)
  (local $22 i32)
  (local $23 i64)
  (local $24 i64)
  (local $25 i64)
  (global.set $global$0
   (local.tee $6
    (i32.add
     (global.get $global$0)
     (i32.const -64)
    )
   )
  )
  (i32.store offset=60
   (local.get $6)
   (local.get $1)
  )
  (local.set $21
   (i32.add
    (local.get $6)
    (i32.const 39)
   )
  )
  (local.set $17
   (i32.add
    (local.get $6)
    (i32.const 40)
   )
  )
  (block $block53
   (block $block17
    (block $block14
     (block $block
      (loop $label4
       (local.set $5
        (i32.const 0)
       )
       (loop $label1
        (local.set $11
         (local.get $1)
        )
        (br_if $block
         (i32.gt_s
          (local.get $5)
          (i32.xor
           (local.get $12)
           (i32.const 2147483647)
          )
         )
        )
        (local.set $12
         (i32.add
          (local.get $5)
          (local.get $12)
         )
        )
        (block $block16
         (block $block19
          (block $block43
           (block $block8
            (if
             (local.tee $9
              (i32.load8_u
               (local.tee $5
                (local.get $1)
               )
              )
             )
             (then
              (loop $label14
               (block $block2
                (block $block1
                 (if
                  (i32.eqz
                   (local.tee $1
                    (i32.and
                     (local.get $9)
                     (i32.const 255)
                    )
                   )
                  )
                  (then
                   (local.set $1
                    (local.get $5)
                   )
                   (br $block1)
                  )
                 )
                 (br_if $block2
                  (i32.ne
                   (local.get $1)
                   (i32.const 37)
                  )
                 )
                 (local.set $9
                  (local.get $5)
                 )
                 (loop $label
                  (if
                   (i32.ne
                    (i32.load8_u offset=1
                     (local.get $9)
                    )
                    (i32.const 37)
                   )
                   (then
                    (local.set $1
                     (local.get $9)
                    )
                    (br $block1)
                   )
                  )
                  (local.set $5
                   (i32.add
                    (local.get $5)
                    (i32.const 1)
                   )
                  )
                  (local.set $8
                   (i32.load8_u offset=2
                    (local.get $9)
                   )
                  )
                  (local.set $9
                   (local.tee $1
                    (i32.add
                     (local.get $9)
                     (i32.const 2)
                    )
                   )
                  )
                  (br_if $label
                   (i32.eq
                    (local.get $8)
                    (i32.const 37)
                   )
                  )
                 )
                )
                (br_if $block
                 (i32.gt_s
                  (local.tee $5
                   (i32.sub
                    (local.get $5)
                    (local.get $11)
                   )
                  )
                  (local.tee $22
                   (i32.xor
                    (local.get $12)
                    (i32.const 2147483647)
                   )
                  )
                 )
                )
                (if
                 (local.get $0)
                 (then
                  (call $14
                   (local.get $0)
                   (local.get $11)
                   (local.get $5)
                  )
                 )
                )
                (br_if $label1
                 (local.get $5)
                )
                (i32.store offset=60
                 (local.get $6)
                 (local.get $1)
                )
                (local.set $5
                 (i32.add
                  (local.get $1)
                  (i32.const 1)
                 )
                )
                (local.set $16
                 (i32.const -1)
                )
                (block $block3
                 (br_if $block3
                  (i32.gt_u
                   (local.tee $8
                    (i32.sub
                     (i32.load8_s offset=1
                      (local.get $1)
                     )
                     (i32.const 48)
                    )
                   )
                   (i32.const 9)
                  )
                 )
                 (br_if $block3
                  (i32.ne
                   (i32.load8_u offset=2
                    (local.get $1)
                   )
                   (i32.const 36)
                  )
                 )
                 (local.set $5
                  (i32.add
                   (local.get $1)
                   (i32.const 3)
                  )
                 )
                 (local.set $18
                  (i32.const 1)
                 )
                 (local.set $16
                  (local.get $8)
                 )
                )
                (i32.store offset=60
                 (local.get $6)
                 (local.get $5)
                )
                (local.set $10
                 (i32.const 0)
                )
                (block $block4
                 (if
                  (i32.gt_u
                   (local.tee $1
                    (i32.sub
                     (local.tee $9
                      (i32.load8_s
                       (local.get $5)
                      )
                     )
                     (i32.const 32)
                    )
                   )
                   (i32.const 31)
                  )
                  (then
                   (local.set $8
                    (local.get $5)
                   )
                   (br $block4)
                  )
                 )
                 (local.set $8
                  (local.get $5)
                 )
                 (br_if $block4
                  (i32.eqz
                   (i32.and
                    (local.tee $1
                     (i32.shl
                      (i32.const 1)
                      (local.get $1)
                     )
                    )
                    (i32.const 75913)
                   )
                  )
                 )
                 (loop $label2
                  (i32.store offset=60
                   (local.get $6)
                   (local.tee $8
                    (i32.add
                     (local.get $5)
                     (i32.const 1)
                    )
                   )
                  )
                  (local.set $10
                   (i32.or
                    (local.get $1)
                    (local.get $10)
                   )
                  )
                  (br_if $block4
                   (i32.ge_u
                    (local.tee $1
                     (i32.sub
                      (local.tee $9
                       (i32.load8_s offset=1
                        (local.get $5)
                       )
                      )
                      (i32.const 32)
                     )
                    )
                    (i32.const 32)
                   )
                  )
                  (local.set $5
                   (local.get $8)
                  )
                  (br_if $label2
                   (i32.and
                    (local.tee $1
                     (i32.shl
                      (i32.const 1)
                      (local.get $1)
                     )
                    )
                    (i32.const 75913)
                   )
                  )
                 )
                )
                (block $block9
                 (if
                  (i32.eq
                   (local.get $9)
                   (i32.const 42)
                  )
                  (then
                   (local.set $18
                    (block $block7 (result i32)
                     (block $block5
                      (br_if $block5
                       (i32.gt_u
                        (local.tee $1
                         (i32.sub
                          (i32.load8_s offset=1
                           (local.get $8)
                          )
                          (i32.const 48)
                         )
                        )
                        (i32.const 9)
                       )
                      )
                      (br_if $block5
                       (i32.ne
                        (i32.load8_u offset=2
                         (local.get $8)
                        )
                        (i32.const 36)
                       )
                      )
                      (local.set $15
                       (block $block6 (result i32)
                        (if
                         (i32.eqz
                          (local.get $0)
                         )
                         (then
                          (i32.store
                           (i32.add
                            (local.get $4)
                            (i32.shl
                             (local.get $1)
                             (i32.const 2)
                            )
                           )
                           (i32.const 10)
                          )
                          (br $block6
                           (i32.const 0)
                          )
                         )
                        )
                        (i32.load
                         (i32.add
                          (local.get $3)
                          (i32.shl
                           (local.get $1)
                           (i32.const 3)
                          )
                         )
                        )
                       )
                      )
                      (local.set $1
                       (i32.add
                        (local.get $8)
                        (i32.const 3)
                       )
                      )
                      (br $block7
                       (i32.const 1)
                      )
                     )
                     (br_if $block8
                      (local.get $18)
                     )
                     (local.set $1
                      (i32.add
                       (local.get $8)
                       (i32.const 1)
                      )
                     )
                     (if
                      (i32.eqz
                       (local.get $0)
                      )
                      (then
                       (i32.store offset=60
                        (local.get $6)
                        (local.get $1)
                       )
                       (local.set $18
                        (i32.const 0)
                       )
                       (local.set $15
                        (i32.const 0)
                       )
                       (br $block9)
                      )
                     )
                     (i32.store
                      (local.get $2)
                      (i32.add
                       (local.tee $5
                        (i32.load
                         (local.get $2)
                        )
                       )
                       (i32.const 4)
                      )
                     )
                     (local.set $15
                      (i32.load
                       (local.get $5)
                      )
                     )
                     (i32.const 0)
                    )
                   )
                   (i32.store offset=60
                    (local.get $6)
                    (local.get $1)
                   )
                   (br_if $block9
                    (i32.ge_s
                     (local.get $15)
                     (i32.const 0)
                    )
                   )
                   (local.set $15
                    (i32.sub
                     (i32.const 0)
                     (local.get $15)
                    )
                   )
                   (local.set $10
                    (i32.or
                     (local.get $10)
                     (i32.const 8192)
                    )
                   )
                   (br $block9)
                  )
                 )
                 (br_if $block
                  (i32.lt_s
                   (local.tee $15
                    (call $15
                     (i32.add
                      (local.get $6)
                      (i32.const 60)
                     )
                    )
                   )
                   (i32.const 0)
                  )
                 )
                 (local.set $1
                  (i32.load offset=60
                   (local.get $6)
                  )
                 )
                )
                (local.set $5
                 (i32.const 0)
                )
                (local.set $7
                 (i32.const -1)
                )
                (local.set $19
                 (block $block10 (result i32)
                  (drop
                   (br_if $block10
                    (i32.const 0)
                    (i32.ne
                     (i32.load8_u
                      (local.get $1)
                     )
                     (i32.const 46)
                    )
                   )
                  )
                  (if
                   (i32.eq
                    (i32.load8_u offset=1
                     (local.get $1)
                    )
                    (i32.const 42)
                   )
                   (then
                    (local.set $7
                     (block $block13 (result i32)
                      (block $block11
                       (br_if $block11
                        (i32.gt_u
                         (local.tee $8
                          (i32.sub
                           (i32.load8_s offset=2
                            (local.get $1)
                           )
                           (i32.const 48)
                          )
                         )
                         (i32.const 9)
                        )
                       )
                       (br_if $block11
                        (i32.ne
                         (i32.load8_u offset=3
                          (local.get $1)
                         )
                         (i32.const 36)
                        )
                       )
                       (local.set $1
                        (i32.add
                         (local.get $1)
                         (i32.const 4)
                        )
                       )
                       (br $block13
                        (block $block12 (result i32)
                         (if
                          (i32.eqz
                           (local.get $0)
                          )
                          (then
                           (i32.store
                            (i32.add
                             (local.get $4)
                             (i32.shl
                              (local.get $8)
                              (i32.const 2)
                             )
                            )
                            (i32.const 10)
                           )
                           (br $block12
                            (i32.const 0)
                           )
                          )
                         )
                         (i32.load
                          (i32.add
                           (local.get $3)
                           (i32.shl
                            (local.get $8)
                            (i32.const 3)
                           )
                          )
                         )
                        )
                       )
                      )
                      (br_if $block8
                       (local.get $18)
                      )
                      (local.set $1
                       (i32.add
                        (local.get $1)
                        (i32.const 2)
                       )
                      )
                      (drop
                       (br_if $block13
                        (i32.const 0)
                        (i32.eqz
                         (local.get $0)
                        )
                       )
                      )
                      (i32.store
                       (local.get $2)
                       (i32.add
                        (local.tee $8
                         (i32.load
                          (local.get $2)
                         )
                        )
                        (i32.const 4)
                       )
                      )
                      (i32.load
                       (local.get $8)
                      )
                     )
                    )
                    (i32.store offset=60
                     (local.get $6)
                     (local.get $1)
                    )
                    (br $block10
                     (i32.ge_s
                      (local.get $7)
                      (i32.const 0)
                     )
                    )
                   )
                  )
                  (i32.store offset=60
                   (local.get $6)
                   (i32.add
                    (local.get $1)
                    (i32.const 1)
                   )
                  )
                  (local.set $7
                   (call $15
                    (i32.add
                     (local.get $6)
                     (i32.const 60)
                    )
                   )
                  )
                  (local.set $1
                   (i32.load offset=60
                    (local.get $6)
                   )
                  )
                  (i32.const 1)
                 )
                )
                (loop $label3
                 (local.set $13
                  (local.get $5)
                 )
                 (local.set $8
                  (i32.const 28)
                 )
                 (br_if $block14
                  (i32.lt_u
                   (i32.sub
                    (local.tee $5
                     (i32.load8_s
                      (local.tee $14
                       (local.get $1)
                      )
                     )
                    )
                    (i32.const 123)
                   )
                   (i32.const -58)
                  )
                 )
                 (local.set $1
                  (i32.add
                   (local.get $1)
                   (i32.const 1)
                  )
                 )
                 (br_if $label3
                  (i32.lt_u
                   (i32.and
                    (i32.sub
                     (local.tee $5
                      (i32.load8_u
                       (i32.add
                        (i32.add
                         (local.get $5)
                         (i32.mul
                          (local.get $13)
                          (i32.const 58)
                         )
                        )
                        (i32.const 1327)
                       )
                      )
                     )
                     (i32.const 1)
                    )
                    (i32.const 255)
                   )
                   (i32.const 8)
                  )
                 )
                )
                (i32.store offset=60
                 (local.get $6)
                 (local.get $1)
                )
                (block $block15
                 (if
                  (i32.ne
                   (local.get $5)
                   (i32.const 27)
                  )
                  (then
                   (br_if $block14
                    (i32.eqz
                     (local.get $5)
                    )
                   )
                   (if
                    (i32.ge_s
                     (local.get $16)
                     (i32.const 0)
                    )
                    (then
                     (if
                      (i32.eqz
                       (local.get $0)
                      )
                      (then
                       (i32.store
                        (i32.add
                         (local.get $4)
                         (i32.shl
                          (local.get $16)
                          (i32.const 2)
                         )
                        )
                        (local.get $5)
                       )
                       (br $label4)
                      )
                     )
                     (i64.store offset=48
                      (local.get $6)
                      (i64.load
                       (i32.add
                        (local.get $3)
                        (i32.shl
                         (local.get $16)
                         (i32.const 3)
                        )
                       )
                      )
                     )
                     (br $block15)
                    )
                   )
                   (br_if $block16
                    (i32.eqz
                     (local.get $0)
                    )
                   )
                   (call $16
                    (i32.add
                     (local.get $6)
                     (i32.const 48)
                    )
                    (local.get $5)
                    (local.get $2)
                   )
                   (br $block15)
                  )
                 )
                 (br_if $block14
                  (i32.ge_s
                   (local.get $16)
                   (i32.const 0)
                  )
                 )
                 (local.set $5
                  (i32.const 0)
                 )
                 (br_if $label1
                  (i32.eqz
                   (local.get $0)
                  )
                 )
                )
                (br_if $block17
                 (i32.and
                  (i32.load8_u
                   (local.get $0)
                  )
                  (i32.const 32)
                 )
                )
                (local.set $10
                 (select
                  (local.tee $9
                   (i32.and
                    (local.get $10)
                    (i32.const -65537)
                   )
                  )
                  (local.get $10)
                  (i32.and
                   (local.get $10)
                   (i32.const 8192)
                  )
                 )
                )
                (local.set $16
                 (i32.const 0)
                )
                (local.set $20
                 (i32.const 1024)
                )
                (local.set $8
                 (local.get $17)
                )
                (block $block20
                 (block $block51
                  (local.set $9
                   (block $block50 (result i32)
                    (block $block49
                     (block $block31
                      (block $block29
                       (block $block26
                        (block $block21
                         (block $block40
                          (local.set $20
                           (block $block32 (result i32)
                            (block $block22
                             (block $block24
                              (block $block18
                               (block $block25
                                (block $block23
                                 (block $block27
                                  (block $block28
                                   (br_table $block18 $block19 $block19 $block19 $block19 $block19 $block19 $block19 $block19 $block20 $block19 $block21 $block22 $block20 $block20 $block20 $block19 $block22 $block19 $block19 $block19 $block19 $block23 $block24 $block25 $block19 $block19 $block26 $block19 $block27 $block19 $block19 $block18 $block28
                                    (i32.sub
                                     (local.tee $5
                                      (select
                                       (select
                                        (i32.and
                                         (local.tee $14
                                          (i32.extend8_s
                                           (local.tee $5
                                            (i32.load8_u
                                             (local.get $14)
                                            )
                                           )
                                          )
                                         )
                                         (i32.const -45)
                                        )
                                        (local.get $14)
                                        (i32.eq
                                         (i32.and
                                          (local.get $5)
                                          (i32.const 15)
                                         )
                                         (i32.const 3)
                                        )
                                       )
                                       (local.get $14)
                                       (local.get $13)
                                      )
                                     )
                                     (i32.const 88)
                                    )
                                   )
                                  )
                                  (block $block30
                                   (br_table $block20 $block19 $block29 $block19 $block20 $block20 $block20 $block30
                                    (i32.sub
                                     (local.get $5)
                                     (i32.const 65)
                                    )
                                   )
                                  )
                                  (br_if $block31
                                   (i32.eq
                                    (local.get $5)
                                    (i32.const 83)
                                   )
                                  )
                                  (br $block19)
                                 )
                                 (local.set $24
                                  (i64.load offset=48
                                   (local.get $6)
                                  )
                                 )
                                 (br $block32
                                  (i32.const 1024)
                                 )
                                )
                                (local.set $5
                                 (i32.const 0)
                                )
                                (block $block39
                                 (block $block38
                                  (block $block37
                                   (block $block36
                                    (block $block35
                                     (block $block34
                                      (block $block33
                                       (br_table $block33 $block34 $block35 $block36 $block37 $label1 $block38 $block39 $label1
                                        (local.get $13)
                                       )
                                      )
                                      (i32.store
                                       (i32.load offset=48
                                        (local.get $6)
                                       )
                                       (local.get $12)
                                      )
                                      (br $label1)
                                     )
                                     (i32.store
                                      (i32.load offset=48
                                       (local.get $6)
                                      )
                                      (local.get $12)
                                     )
                                     (br $label1)
                                    )
                                    (i64.store
                                     (i32.load offset=48
                                      (local.get $6)
                                     )
                                     (i64.extend_i32_s
                                      (local.get $12)
                                     )
                                    )
                                    (br $label1)
                                   )
                                   (i32.store16
                                    (i32.load offset=48
                                     (local.get $6)
                                    )
                                    (local.get $12)
                                   )
                                   (br $label1)
                                  )
                                  (i32.store8
                                   (i32.load offset=48
                                    (local.get $6)
                                   )
                                   (local.get $12)
                                  )
                                  (br $label1)
                                 )
                                 (i32.store
                                  (i32.load offset=48
                                   (local.get $6)
                                  )
                                  (local.get $12)
                                 )
                                 (br $label1)
                                )
                                (i64.store
                                 (i32.load offset=48
                                  (local.get $6)
                                 )
                                 (i64.extend_i32_s
                                  (local.get $12)
                                 )
                                )
                                (br $label1)
                               )
                               (local.set $7
                                (select
                                 (i32.const 8)
                                 (local.get $7)
                                 (i32.le_u
                                  (local.get $7)
                                  (i32.const 8)
                                 )
                                )
                               )
                               (local.set $10
                                (i32.or
                                 (local.get $10)
                                 (i32.const 8)
                                )
                               )
                               (local.set $5
                                (i32.const 120)
                               )
                              )
                              (local.set $1
                               (local.get $17)
                              )
                              (if
                               (i64.ne
                                (local.tee $23
                                 (local.tee $24
                                  (i64.load offset=48
                                   (local.get $6)
                                  )
                                 )
                                )
                                (i64.const 0)
                               )
                               (then
                                (local.set $9
                                 (i32.and
                                  (local.get $5)
                                  (i32.const 32)
                                 )
                                )
                                (loop $label5
                                 (i32.store8
                                  (local.tee $1
                                   (i32.sub
                                    (local.get $1)
                                    (i32.const 1)
                                   )
                                  )
                                  (i32.or
                                   (i32.load8_u
                                    (i32.add
                                     (i32.and
                                      (i32.wrap_i64
                                       (local.get $23)
                                      )
                                      (i32.const 15)
                                     )
                                     (i32.const 1856)
                                    )
                                   )
                                   (local.get $9)
                                  )
                                 )
                                 (local.set $11
                                  (i64.gt_u
                                   (local.get $23)
                                   (i64.const 15)
                                  )
                                 )
                                 (local.set $23
                                  (i64.shr_u
                                   (local.get $23)
                                   (i64.const 4)
                                  )
                                 )
                                 (br_if $label5
                                  (local.get $11)
                                 )
                                )
                               )
                              )
                              (local.set $11
                               (local.get $1)
                              )
                              (br_if $block40
                               (i64.eqz
                                (local.get $24)
                               )
                              )
                              (br_if $block40
                               (i32.eqz
                                (i32.and
                                 (local.get $10)
                                 (i32.const 8)
                                )
                               )
                              )
                              (local.set $20
                               (i32.add
                                (i32.shr_u
                                 (local.get $5)
                                 (i32.const 4)
                                )
                                (i32.const 1024)
                               )
                              )
                              (local.set $16
                               (i32.const 2)
                              )
                              (br $block40)
                             )
                             (local.set $1
                              (local.get $17)
                             )
                             (if
                              (i64.ne
                               (local.tee $23
                                (local.tee $24
                                 (i64.load offset=48
                                  (local.get $6)
                                 )
                                )
                               )
                               (i64.const 0)
                              )
                              (then
                               (loop $label6
                                (i32.store8
                                 (local.tee $1
                                  (i32.sub
                                   (local.get $1)
                                   (i32.const 1)
                                  )
                                 )
                                 (i32.or
                                  (i32.and
                                   (i32.wrap_i64
                                    (local.get $23)
                                   )
                                   (i32.const 7)
                                  )
                                  (i32.const 48)
                                 )
                                )
                                (local.set $5
                                 (i64.gt_u
                                  (local.get $23)
                                  (i64.const 7)
                                 )
                                )
                                (local.set $23
                                 (i64.shr_u
                                  (local.get $23)
                                  (i64.const 3)
                                 )
                                )
                                (br_if $label6
                                 (local.get $5)
                                )
                               )
                              )
                             )
                             (local.set $11
                              (local.get $1)
                             )
                             (br_if $block40
                              (i32.eqz
                               (i32.and
                                (local.get $10)
                                (i32.const 8)
                               )
                              )
                             )
                             (local.set $7
                              (select
                               (local.get $7)
                               (i32.add
                                (local.tee $1
                                 (i32.sub
                                  (local.get $17)
                                  (local.get $1)
                                 )
                                )
                                (i32.const 1)
                               )
                               (i32.lt_s
                                (local.get $1)
                                (local.get $7)
                               )
                              )
                             )
                             (br $block40)
                            )
                            (if
                             (i64.lt_s
                              (local.tee $24
                               (i64.load offset=48
                                (local.get $6)
                               )
                              )
                              (i64.const 0)
                             )
                             (then
                              (i64.store offset=48
                               (local.get $6)
                               (local.tee $24
                                (i64.sub
                                 (i64.const 0)
                                 (local.get $24)
                                )
                               )
                              )
                              (local.set $16
                               (i32.const 1)
                              )
                              (br $block32
                               (i32.const 1024)
                              )
                             )
                            )
                            (if
                             (i32.and
                              (local.get $10)
                              (i32.const 2048)
                             )
                             (then
                              (local.set $16
                               (i32.const 1)
                              )
                              (br $block32
                               (i32.const 1025)
                              )
                             )
                            )
                            (select
                             (i32.const 1026)
                             (i32.const 1024)
                             (local.tee $16
                              (i32.and
                               (local.get $10)
                               (i32.const 1)
                              )
                             )
                            )
                           )
                          )
                          (local.set $1
                           (local.get $17)
                          )
                          (block $block41
                           (if
                            (i64.lt_u
                             (local.tee $23
                              (local.get $24)
                             )
                             (i64.const 4294967296)
                            )
                            (then
                             (local.set $25
                              (local.get $23)
                             )
                             (br $block41)
                            )
                           )
                           (loop $label7
                            (i32.store8
                             (local.tee $1
                              (i32.sub
                               (local.get $1)
                               (i32.const 1)
                              )
                             )
                             (i32.or
                              (i32.wrap_i64
                               (i64.sub
                                (local.get $23)
                                (i64.mul
                                 (local.tee $25
                                  (i64.div_u
                                   (local.get $23)
                                   (i64.const 10)
                                  )
                                 )
                                 (i64.const 10)
                                )
                               )
                              )
                              (i32.const 48)
                             )
                            )
                            (local.set $5
                             (i64.gt_u
                              (local.get $23)
                              (i64.const 42949672959)
                             )
                            )
                            (local.set $23
                             (local.get $25)
                            )
                            (br_if $label7
                             (local.get $5)
                            )
                           )
                          )
                          (if
                           (i64.ne
                            (local.get $25)
                            (i64.const 0)
                           )
                           (then
                            (local.set $5
                             (i32.wrap_i64
                              (local.get $25)
                             )
                            )
                            (loop $label8
                             (i32.store8
                              (local.tee $1
                               (i32.sub
                                (local.get $1)
                                (i32.const 1)
                               )
                              )
                              (i32.or
                               (i32.sub
                                (local.get $5)
                                (i32.mul
                                 (local.tee $11
                                  (i32.div_u
                                   (local.get $5)
                                   (i32.const 10)
                                  )
                                 )
                                 (i32.const 10)
                                )
                               )
                               (i32.const 48)
                              )
                             )
                             (local.set $9
                              (i32.gt_u
                               (local.get $5)
                               (i32.const 9)
                              )
                             )
                             (local.set $5
                              (local.get $11)
                             )
                             (br_if $label8
                              (local.get $9)
                             )
                            )
                           )
                          )
                          (local.set $11
                           (local.get $1)
                          )
                         )
                         (br_if $block
                          (i32.and
                           (local.get $19)
                           (i32.lt_s
                            (local.get $7)
                            (i32.const 0)
                           )
                          )
                         )
                         (local.set $10
                          (select
                           (i32.and
                            (local.get $10)
                            (i32.const -65537)
                           )
                           (local.get $10)
                           (local.get $19)
                          )
                         )
                         (block $block42
                          (br_if $block42
                           (i64.ne
                            (local.get $24)
                            (i64.const 0)
                           )
                          )
                          (br_if $block42
                           (local.get $7)
                          )
                          (local.set $11
                           (local.get $17)
                          )
                          (local.set $7
                           (i32.const 0)
                          )
                          (br $block19)
                         )
                         (local.set $7
                          (select
                           (local.get $7)
                           (local.tee $1
                            (i32.add
                             (i64.eqz
                              (local.get $24)
                             )
                             (i32.sub
                              (local.get $17)
                              (local.get $11)
                             )
                            )
                           )
                           (i32.lt_s
                            (local.get $1)
                            (local.get $7)
                           )
                          )
                         )
                         (br $block19)
                        )
                        (local.set $5
                         (i32.load8_u offset=48
                          (local.get $6)
                         )
                        )
                        (br $block43)
                       )
                       (local.set $8
                        (i32.add
                         (local.tee $1
                          (select
                           (i32.sub
                            (local.tee $1
                             (block $block48 (result i32)
                              (local.set $10
                               (i32.ne
                                (local.tee $14
                                 (local.tee $8
                                  (select
                                   (i32.const 2147483647)
                                   (local.get $7)
                                   (i32.ge_u
                                    (local.get $7)
                                    (i32.const 2147483647)
                                   )
                                  )
                                 )
                                )
                                (i32.const 0)
                               )
                              )
                              (block $block46
                               (block $block45
                                (block $block44
                                 (br_if $block44
                                  (i32.eqz
                                   (i32.and
                                    (local.tee $13
                                     (local.tee $5
                                      (local.tee $11
                                       (select
                                        (local.tee $1
                                         (i32.load offset=48
                                          (local.get $6)
                                         )
                                        )
                                        (i32.const 1113)
                                        (local.get $1)
                                       )
                                      )
                                     )
                                    )
                                    (i32.const 3)
                                   )
                                  )
                                 )
                                 (br_if $block44
                                  (i32.eqz
                                   (local.get $14)
                                  )
                                 )
                                 (loop $label9
                                  (br_if $block45
                                   (i32.eqz
                                    (i32.load8_u
                                     (local.get $13)
                                    )
                                   )
                                  )
                                  (local.set $10
                                   (i32.ne
                                    (local.tee $14
                                     (i32.sub
                                      (local.get $14)
                                      (i32.const 1)
                                     )
                                    )
                                    (i32.const 0)
                                   )
                                  )
                                  (br_if $block44
                                   (i32.eqz
                                    (i32.and
                                     (local.tee $13
                                      (i32.add
                                       (local.get $13)
                                       (i32.const 1)
                                      )
                                     )
                                     (i32.const 3)
                                    )
                                   )
                                  )
                                  (br_if $label9
                                   (local.get $14)
                                  )
                                 )
                                )
                                (br_if $block46
                                 (i32.eqz
                                  (local.get $10)
                                 )
                                )
                                (block $block47
                                 (br_if $block47
                                  (i32.eqz
                                   (i32.load8_u
                                    (local.get $13)
                                   )
                                  )
                                 )
                                 (br_if $block47
                                  (i32.lt_u
                                   (local.get $14)
                                   (i32.const 4)
                                  )
                                 )
                                 (loop $label10
                                  (br_if $block45
                                   (i32.ne
                                    (i32.and
                                     (i32.or
                                      (i32.sub
                                       (i32.const 16843008)
                                       (local.tee $1
                                        (i32.load
                                         (local.get $13)
                                        )
                                       )
                                      )
                                      (local.get $1)
                                     )
                                     (i32.const -2139062144)
                                    )
                                    (i32.const -2139062144)
                                   )
                                  )
                                  (local.set $13
                                   (i32.add
                                    (local.get $13)
                                    (i32.const 4)
                                   )
                                  )
                                  (br_if $label10
                                   (i32.gt_u
                                    (local.tee $14
                                     (i32.sub
                                      (local.get $14)
                                      (i32.const 4)
                                     )
                                    )
                                    (i32.const 3)
                                   )
                                  )
                                 )
                                )
                                (br_if $block46
                                 (i32.eqz
                                  (local.get $14)
                                 )
                                )
                               )
                               (loop $label11
                                (drop
                                 (br_if $block48
                                  (local.get $13)
                                  (i32.eqz
                                   (i32.load8_u
                                    (local.get $13)
                                   )
                                  )
                                 )
                                )
                                (local.set $13
                                 (i32.add
                                  (local.get $13)
                                  (i32.const 1)
                                 )
                                )
                                (br_if $label11
                                 (local.tee $14
                                  (i32.sub
                                   (local.get $14)
                                   (i32.const 1)
                                  )
                                 )
                                )
                               )
                              )
                              (i32.const 0)
                             )
                            )
                            (local.get $5)
                           )
                           (local.get $8)
                           (local.get $1)
                          )
                         )
                         (local.get $11)
                        )
                       )
                       (if
                        (i32.ge_s
                         (local.get $7)
                         (i32.const 0)
                        )
                        (then
                         (local.set $10
                          (local.get $9)
                         )
                         (local.set $7
                          (local.get $1)
                         )
                         (br $block19)
                        )
                       )
                       (local.set $10
                        (local.get $9)
                       )
                       (local.set $7
                        (local.get $1)
                       )
                       (br_if $block
                        (i32.load8_u
                         (local.get $8)
                        )
                       )
                       (br $block19)
                      )
                      (br_if $block49
                       (i64.ne
                        (local.tee $23
                         (i64.load offset=48
                          (local.get $6)
                         )
                        )
                        (i64.const 0)
                       )
                      )
                      (local.set $5
                       (i32.const 0)
                      )
                      (br $block43)
                     )
                     (if
                      (local.get $7)
                      (then
                       (br $block50
                        (i32.load offset=48
                         (local.get $6)
                        )
                       )
                      )
                     )
                     (local.set $5
                      (i32.const 0)
                     )
                     (call $17
                      (local.get $0)
                      (i32.const 32)
                      (local.get $15)
                      (i32.const 0)
                      (local.get $10)
                     )
                     (br $block51)
                    )
                    (i32.store offset=12
                     (local.get $6)
                     (i32.const 0)
                    )
                    (i64.store32 offset=8
                     (local.get $6)
                     (local.get $23)
                    )
                    (i32.store offset=48
                     (local.get $6)
                     (local.tee $5
                      (i32.add
                       (local.get $6)
                       (i32.const 8)
                      )
                     )
                    )
                    (local.set $7
                     (i32.const -1)
                    )
                    (local.get $5)
                   )
                  )
                  (local.set $5
                   (i32.const 0)
                  )
                  (loop $label12
                   (block $block52
                    (br_if $block52
                     (i32.eqz
                      (local.tee $11
                       (i32.load
                        (local.get $9)
                       )
                      )
                     )
                    )
                    (br_if $block17
                     (i32.lt_s
                      (local.tee $11
                       (call $18
                        (i32.add
                         (local.get $6)
                         (i32.const 4)
                        )
                        (local.get $11)
                       )
                      )
                      (i32.const 0)
                     )
                    )
                    (br_if $block52
                     (i32.gt_u
                      (local.get $11)
                      (i32.sub
                       (local.get $7)
                       (local.get $5)
                      )
                     )
                    )
                    (local.set $9
                     (i32.add
                      (local.get $9)
                      (i32.const 4)
                     )
                    )
                    (br_if $label12
                     (i32.lt_u
                      (local.tee $5
                       (i32.add
                        (local.get $5)
                        (local.get $11)
                       )
                      )
                      (local.get $7)
                     )
                    )
                   )
                  )
                  (local.set $8
                   (i32.const 61)
                  )
                  (br_if $block14
                   (i32.lt_s
                    (local.get $5)
                    (i32.const 0)
                   )
                  )
                  (call $17
                   (local.get $0)
                   (i32.const 32)
                   (local.get $15)
                   (local.get $5)
                   (local.get $10)
                  )
                  (if
                   (i32.eqz
                    (local.get $5)
                   )
                   (then
                    (local.set $5
                     (i32.const 0)
                    )
                    (br $block51)
                   )
                  )
                  (local.set $8
                   (i32.const 0)
                  )
                  (local.set $9
                   (i32.load offset=48
                    (local.get $6)
                   )
                  )
                  (loop $label13
                   (br_if $block51
                    (i32.eqz
                     (local.tee $11
                      (i32.load
                       (local.get $9)
                      )
                     )
                    )
                   )
                   (br_if $block51
                    (i32.gt_u
                     (local.tee $8
                      (i32.add
                       (local.tee $11
                        (call $18
                         (local.tee $7
                          (i32.add
                           (local.get $6)
                           (i32.const 4)
                          )
                         )
                         (local.get $11)
                        )
                       )
                       (local.get $8)
                      )
                     )
                     (local.get $5)
                    )
                   )
                   (call $14
                    (local.get $0)
                    (local.get $7)
                    (local.get $11)
                   )
                   (local.set $9
                    (i32.add
                     (local.get $9)
                     (i32.const 4)
                    )
                   )
                   (br_if $label13
                    (i32.gt_u
                     (local.get $5)
                     (local.get $8)
                    )
                   )
                  )
                 )
                 (call $17
                  (local.get $0)
                  (i32.const 32)
                  (local.get $15)
                  (local.get $5)
                  (i32.xor
                   (local.get $10)
                   (i32.const 8192)
                  )
                 )
                 (local.set $5
                  (select
                   (local.get $15)
                   (local.get $5)
                   (i32.lt_s
                    (local.get $5)
                    (local.get $15)
                   )
                  )
                 )
                 (br $label1)
                )
                (br_if $block
                 (i32.and
                  (local.get $19)
                  (i32.lt_s
                   (local.get $7)
                   (i32.const 0)
                  )
                 )
                )
                (local.set $8
                 (i32.const 61)
                )
                (drop
                 (f64.load offset=48
                  (local.get $6)
                 )
                )
                (unreachable)
               )
               (local.set $9
                (i32.load8_u offset=1
                 (local.get $5)
                )
               )
               (local.set $5
                (i32.add
                 (local.get $5)
                 (i32.const 1)
                )
               )
               (br $label14)
              )
              (unreachable)
             )
            )
            (br_if $block53
             (local.get $0)
            )
            (br_if $block16
             (i32.eqz
              (local.get $18)
             )
            )
            (local.set $5
             (i32.const 1)
            )
            (loop $label15
             (if
              (local.tee $0
               (i32.load
                (i32.add
                 (local.get $4)
                 (i32.shl
                  (local.get $5)
                  (i32.const 2)
                 )
                )
               )
              )
              (then
               (call $16
                (i32.add
                 (local.get $3)
                 (i32.shl
                  (local.get $5)
                  (i32.const 3)
                 )
                )
                (local.get $0)
                (local.get $2)
               )
               (local.set $12
                (i32.const 1)
               )
               (br_if $label15
                (i32.ne
                 (local.tee $5
                  (i32.add
                   (local.get $5)
                   (i32.const 1)
                  )
                 )
                 (i32.const 10)
                )
               )
               (br $block53)
              )
             )
            )
            (if
             (i32.ge_u
              (local.get $5)
              (i32.const 10)
             )
             (then
              (local.set $12
               (i32.const 1)
              )
              (br $block53)
             )
            )
            (loop $label16
             (br_if $block8
              (i32.load
               (i32.add
                (local.get $4)
                (i32.shl
                 (local.get $5)
                 (i32.const 2)
                )
               )
              )
             )
             (local.set $12
              (i32.const 1)
             )
             (br_if $label16
              (i32.ne
               (local.tee $5
                (i32.add
                 (local.get $5)
                 (i32.const 1)
                )
               )
               (i32.const 10)
              )
             )
            )
            (br $block53)
           )
           (local.set $8
            (i32.const 28)
           )
           (br $block14)
          )
          (i32.store8 offset=39
           (local.get $6)
           (local.get $5)
          )
          (local.set $7
           (i32.const 1)
          )
          (local.set $11
           (local.get $21)
          )
          (local.set $10
           (local.get $9)
          )
         )
         (br_if $block
          (i32.gt_s
           (local.tee $1
            (select
             (local.get $7)
             (local.tee $9
              (i32.sub
               (local.get $8)
               (local.get $11)
              )
             )
             (i32.gt_s
              (local.get $7)
              (local.get $9)
             )
            )
           )
           (i32.xor
            (local.get $16)
            (i32.const 2147483647)
           )
          )
         )
         (local.set $8
          (i32.const 61)
         )
         (br_if $block14
          (i32.gt_s
           (local.tee $5
            (select
             (local.get $15)
             (local.tee $7
              (i32.add
               (local.get $1)
               (local.get $16)
              )
             )
             (i32.lt_s
              (local.get $7)
              (local.get $15)
             )
            )
           )
           (local.get $22)
          )
         )
         (call $17
          (local.get $0)
          (i32.const 32)
          (local.get $5)
          (local.get $7)
          (local.get $10)
         )
         (call $14
          (local.get $0)
          (local.get $20)
          (local.get $16)
         )
         (call $17
          (local.get $0)
          (i32.const 48)
          (local.get $5)
          (local.get $7)
          (i32.xor
           (local.get $10)
           (i32.const 65536)
          )
         )
         (call $17
          (local.get $0)
          (i32.const 48)
          (local.get $1)
          (local.get $9)
          (i32.const 0)
         )
         (call $14
          (local.get $0)
          (local.get $11)
          (local.get $9)
         )
         (call $17
          (local.get $0)
          (i32.const 32)
          (local.get $5)
          (local.get $7)
          (i32.xor
           (local.get $10)
           (i32.const 8192)
          )
         )
         (local.set $1
          (i32.load offset=60
           (local.get $6)
          )
         )
         (br $label1)
        )
       )
      )
      (local.set $12
       (i32.const 0)
      )
      (br $block53)
     )
     (local.set $8
      (i32.const 61)
     )
    )
    (i32.store
     (i32.const 2220)
     (local.get $8)
    )
   )
   (local.set $12
    (i32.const -1)
   )
  )
  (global.set $global$0
   (i32.sub
    (local.get $6)
    (i32.const -64)
   )
  )
  (local.get $12)
 )
 (func $14 (param $0 i32) (param $1 i32) (param $2 i32)
  (if
   (i32.eqz
    (i32.and
     (i32.load8_u
      (local.get $0)
     )
     (i32.const 32)
    )
   )
   (then
    (drop
     (call $9
      (local.get $1)
      (local.get $2)
      (local.get $0)
     )
    )
   )
  )
 )
 (func $15 (param $0 i32) (result i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (if
   (i32.gt_u
    (local.tee $1
     (i32.sub
      (i32.load8_s
       (local.tee $3
        (i32.load
         (local.get $0)
        )
       )
      )
      (i32.const 48)
     )
    )
    (i32.const 9)
   )
   (then
    (return
     (i32.const 0)
    )
   )
  )
  (loop $label
   (local.set $4
    (i32.const -1)
   )
   (if
    (i32.le_u
     (local.get $2)
     (i32.const 214748364)
    )
    (then
     (local.set $4
      (select
       (i32.const -1)
       (i32.add
        (local.get $1)
        (local.tee $5
         (i32.mul
          (local.get $2)
          (i32.const 10)
         )
        )
       )
       (i32.gt_u
        (local.get $1)
        (i32.xor
         (local.get $5)
         (i32.const 2147483647)
        )
       )
      )
     )
    )
   )
   (i32.store
    (local.get $0)
    (local.tee $5
     (i32.add
      (local.get $3)
      (i32.const 1)
     )
    )
   )
   (local.set $1
    (i32.load8_s offset=1
     (local.get $3)
    )
   )
   (local.set $2
    (local.get $4)
   )
   (local.set $3
    (local.get $5)
   )
   (br_if $label
    (i32.lt_u
     (local.tee $1
      (i32.sub
       (local.get $1)
       (i32.const 48)
      )
     )
     (i32.const 10)
    )
   )
  )
  (local.get $2)
 )
 (func $16 (param $0 i32) (param $1 i32) (param $2 i32)
  (block $block3
   (block $block2
    (block $block1
     (block $block10
      (block $block9
       (block $block8
        (block $block7
         (block $block6
          (block $block5
           (block $block4
            (block $block
             (br_table $block $block1 $block2 $block3 $block1 $block2 $block4 $block5 $block6 $block7 $block3 $block2 $block3 $block3 $block1 $block2 $block8 $block9 $block10
              (i32.sub
               (local.get $1)
               (i32.const 9)
              )
             )
            )
            (i32.store
             (local.get $2)
             (i32.add
              (local.tee $1
               (i32.load
                (local.get $2)
               )
              )
              (i32.const 4)
             )
            )
            (i32.store
             (local.get $0)
             (i32.load
              (local.get $1)
             )
            )
            (return)
           )
           (i32.store
            (local.get $2)
            (i32.add
             (local.tee $1
              (i32.load
               (local.get $2)
              )
             )
             (i32.const 4)
            )
           )
           (i64.store
            (local.get $0)
            (i64.load16_s
             (local.get $1)
            )
           )
           (return)
          )
          (i32.store
           (local.get $2)
           (i32.add
            (local.tee $1
             (i32.load
              (local.get $2)
             )
            )
            (i32.const 4)
           )
          )
          (i64.store
           (local.get $0)
           (i64.load16_u
            (local.get $1)
           )
          )
          (return)
         )
         (i32.store
          (local.get $2)
          (i32.add
           (local.tee $1
            (i32.load
             (local.get $2)
            )
           )
           (i32.const 4)
          )
         )
         (i64.store
          (local.get $0)
          (i64.load8_s
           (local.get $1)
          )
         )
         (return)
        )
        (i32.store
         (local.get $2)
         (i32.add
          (local.tee $1
           (i32.load
            (local.get $2)
           )
          )
          (i32.const 4)
         )
        )
        (i64.store
         (local.get $0)
         (i64.load8_u
          (local.get $1)
         )
        )
        (return)
       )
       (i32.store
        (local.get $2)
        (i32.add
         (local.tee $1
          (i32.and
           (i32.add
            (i32.load
             (local.get $2)
            )
            (i32.const 7)
           )
           (i32.const -8)
          )
         )
         (i32.const 8)
        )
       )
       (f64.store
        (local.get $0)
        (f64.load
         (local.get $1)
        )
       )
       (return)
      )
      (unreachable)
     )
     (return)
    )
    (i32.store
     (local.get $2)
     (i32.add
      (local.tee $1
       (i32.load
        (local.get $2)
       )
      )
      (i32.const 4)
     )
    )
    (i64.store
     (local.get $0)
     (i64.load32_s
      (local.get $1)
     )
    )
    (return)
   )
   (i32.store
    (local.get $2)
    (i32.add
     (local.tee $1
      (i32.load
       (local.get $2)
      )
     )
     (i32.const 4)
    )
   )
   (i64.store
    (local.get $0)
    (i64.load32_u
     (local.get $1)
    )
   )
   (return)
  )
  (i32.store
   (local.get $2)
   (i32.add
    (local.tee $1
     (i32.and
      (i32.add
       (i32.load
        (local.get $2)
       )
       (i32.const 7)
      )
      (i32.const -8)
     )
    )
    (i32.const 8)
   )
  )
  (i64.store
   (local.get $0)
   (i64.load
    (local.get $1)
   )
  )
 )
 (func $17 (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (param $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local $9 i64)
  (global.set $global$0
   (local.tee $6
    (i32.sub
     (global.get $global$0)
     (i32.const 256)
    )
   )
  )
  (block $block
   (br_if $block
    (i32.le_s
     (local.get $2)
     (local.get $3)
    )
   )
   (br_if $block
    (i32.and
     (local.get $4)
     (i32.const 73728)
    )
   )
   (block $block1
    (br_if $block1
     (i32.eqz
      (local.tee $8
       (select
        (local.tee $3
         (i32.sub
          (local.get $2)
          (local.get $3)
         )
        )
        (i32.const 256)
        (local.tee $4
         (i32.lt_u
          (local.get $3)
          (i32.const 256)
         )
        )
       )
      )
     )
    )
    (i32.store8
     (local.get $6)
     (local.get $1)
    )
    (i32.store8
     (i32.sub
      (local.tee $2
       (i32.add
        (local.get $6)
        (local.get $8)
       )
      )
      (i32.const 1)
     )
     (local.get $1)
    )
    (br_if $block1
     (i32.lt_u
      (local.get $8)
      (i32.const 3)
     )
    )
    (i32.store8 offset=2
     (local.get $6)
     (local.get $1)
    )
    (i32.store8 offset=1
     (local.get $6)
     (local.get $1)
    )
    (i32.store8
     (i32.sub
      (local.get $2)
      (i32.const 3)
     )
     (local.get $1)
    )
    (i32.store8
     (i32.sub
      (local.get $2)
      (i32.const 2)
     )
     (local.get $1)
    )
    (br_if $block1
     (i32.lt_u
      (local.get $8)
      (i32.const 7)
     )
    )
    (i32.store8 offset=3
     (local.get $6)
     (local.get $1)
    )
    (i32.store8
     (i32.sub
      (local.get $2)
      (i32.const 4)
     )
     (local.get $1)
    )
    (br_if $block1
     (i32.lt_u
      (local.get $8)
      (i32.const 9)
     )
    )
    (i32.store
     (local.tee $7
      (i32.add
       (local.get $6)
       (local.tee $2
        (i32.and
         (i32.sub
          (i32.const 0)
          (local.get $6)
         )
         (i32.const 3)
        )
       )
      )
     )
     (local.tee $5
      (i32.mul
       (i32.and
        (local.get $1)
        (i32.const 255)
       )
       (i32.const 16843009)
      )
     )
    )
    (i32.store
     (i32.sub
      (local.tee $2
       (i32.add
        (local.get $7)
        (local.tee $1
         (i32.and
          (i32.sub
           (local.get $8)
           (local.get $2)
          )
          (i32.const -4)
         )
        )
       )
      )
      (i32.const 4)
     )
     (local.get $5)
    )
    (br_if $block1
     (i32.lt_u
      (local.get $1)
      (i32.const 9)
     )
    )
    (i32.store offset=8
     (local.get $7)
     (local.get $5)
    )
    (i32.store offset=4
     (local.get $7)
     (local.get $5)
    )
    (i32.store
     (i32.sub
      (local.get $2)
      (i32.const 8)
     )
     (local.get $5)
    )
    (i32.store
     (i32.sub
      (local.get $2)
      (i32.const 12)
     )
     (local.get $5)
    )
    (br_if $block1
     (i32.lt_u
      (local.get $1)
      (i32.const 25)
     )
    )
    (i32.store offset=24
     (local.get $7)
     (local.get $5)
    )
    (i32.store offset=20
     (local.get $7)
     (local.get $5)
    )
    (i32.store offset=16
     (local.get $7)
     (local.get $5)
    )
    (i32.store offset=12
     (local.get $7)
     (local.get $5)
    )
    (i32.store
     (i32.sub
      (local.get $2)
      (i32.const 16)
     )
     (local.get $5)
    )
    (i32.store
     (i32.sub
      (local.get $2)
      (i32.const 20)
     )
     (local.get $5)
    )
    (i32.store
     (i32.sub
      (local.get $2)
      (i32.const 24)
     )
     (local.get $5)
    )
    (i32.store
     (i32.sub
      (local.get $2)
      (i32.const 28)
     )
     (local.get $5)
    )
    (br_if $block1
     (i32.lt_u
      (local.tee $2
       (i32.sub
        (local.get $1)
        (local.tee $1
         (i32.or
          (i32.and
           (local.get $7)
           (i32.const 4)
          )
          (i32.const 24)
         )
        )
       )
      )
      (i32.const 32)
     )
    )
    (local.set $9
     (i64.mul
      (i64.extend_i32_u
       (local.get $5)
      )
      (i64.const 4294967297)
     )
    )
    (local.set $1
     (i32.add
      (local.get $1)
      (local.get $7)
     )
    )
    (loop $label
     (i64.store offset=24
      (local.get $1)
      (local.get $9)
     )
     (i64.store offset=16
      (local.get $1)
      (local.get $9)
     )
     (i64.store offset=8
      (local.get $1)
      (local.get $9)
     )
     (i64.store
      (local.get $1)
      (local.get $9)
     )
     (local.set $1
      (i32.add
       (local.get $1)
       (i32.const 32)
      )
     )
     (br_if $label
      (i32.gt_u
       (local.tee $2
        (i32.sub
         (local.get $2)
         (i32.const 32)
        )
       )
       (i32.const 31)
      )
     )
    )
   )
   (if
    (i32.eqz
     (local.get $4)
    )
    (then
     (loop $label1
      (call $14
       (local.get $0)
       (local.get $6)
       (i32.const 256)
      )
      (br_if $label1
       (i32.gt_u
        (local.tee $3
         (i32.sub
          (local.get $3)
          (i32.const 256)
         )
        )
        (i32.const 255)
       )
      )
     )
    )
   )
   (call $14
    (local.get $0)
    (local.get $6)
    (local.get $3)
   )
  )
  (global.set $global$0
   (i32.add
    (local.get $6)
    (i32.const 256)
   )
  )
 )
 (func $18 (param $0 i32) (param $1 i32) (result i32)
  (if
   (i32.eqz
    (local.get $0)
   )
   (then
    (return
     (i32.const 0)
    )
   )
  )
  (block $block2 (result i32)
   (block $block
    (br $block2
     (if (result i32)
      (local.get $0)
      (then
       (br_if $block
        (i32.le_u
         (local.get $1)
         (i32.const 127)
        )
       )
       (block $block1
        (if
         (i32.eqz
          (i32.load
           (i32.load
            (i32.const 2184)
           )
          )
         )
         (then
          (br_if $block
           (i32.eq
            (i32.and
             (local.get $1)
             (i32.const -128)
            )
            (i32.const 57216)
           )
          )
          (br $block1)
         )
        )
        (if
         (i32.le_u
          (local.get $1)
          (i32.const 2047)
         )
         (then
          (i32.store8 offset=1
           (local.get $0)
           (i32.or
            (i32.and
             (local.get $1)
             (i32.const 63)
            )
            (i32.const 128)
           )
          )
          (i32.store8
           (local.get $0)
           (i32.or
            (i32.shr_u
             (local.get $1)
             (i32.const 6)
            )
            (i32.const 192)
           )
          )
          (br $block2
           (i32.const 2)
          )
         )
        )
        (if
         (i32.eqz
          (i32.and
           (i32.ne
            (i32.and
             (local.get $1)
             (i32.const -8192)
            )
            (i32.const 57344)
           )
           (i32.ge_u
            (local.get $1)
            (i32.const 55296)
           )
          )
         )
         (then
          (i32.store8 offset=2
           (local.get $0)
           (i32.or
            (i32.and
             (local.get $1)
             (i32.const 63)
            )
            (i32.const 128)
           )
          )
          (i32.store8
           (local.get $0)
           (i32.or
            (i32.shr_u
             (local.get $1)
             (i32.const 12)
            )
            (i32.const 224)
           )
          )
          (i32.store8 offset=1
           (local.get $0)
           (i32.or
            (i32.and
             (i32.shr_u
              (local.get $1)
              (i32.const 6)
             )
             (i32.const 63)
            )
            (i32.const 128)
           )
          )
          (br $block2
           (i32.const 3)
          )
         )
        )
        (if
         (i32.le_u
          (i32.sub
           (local.get $1)
           (i32.const 65536)
          )
          (i32.const 1048575)
         )
         (then
          (i32.store8 offset=3
           (local.get $0)
           (i32.or
            (i32.and
             (local.get $1)
             (i32.const 63)
            )
            (i32.const 128)
           )
          )
          (i32.store8
           (local.get $0)
           (i32.or
            (i32.shr_u
             (local.get $1)
             (i32.const 18)
            )
            (i32.const 240)
           )
          )
          (i32.store8 offset=2
           (local.get $0)
           (i32.or
            (i32.and
             (i32.shr_u
              (local.get $1)
              (i32.const 6)
             )
             (i32.const 63)
            )
            (i32.const 128)
           )
          )
          (i32.store8 offset=1
           (local.get $0)
           (i32.or
            (i32.and
             (i32.shr_u
              (local.get $1)
              (i32.const 12)
             )
             (i32.const 63)
            )
            (i32.const 128)
           )
          )
          (br $block2
           (i32.const 4)
          )
         )
        )
       )
       (i32.store
        (i32.const 2220)
        (i32.const 25)
       )
       (i32.const -1)
      )
      (else
       (i32.const 1)
      )
     )
    )
   )
   (i32.store8
    (local.get $0)
    (local.get $1)
   )
   (i32.const 1)
  )
 )
 (func $19 (param $0 i32) (result i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local $9 i32)
  (local $10 i32)
  (local $11 i32)
  (global.set $global$0
   (local.tee $10
    (i32.sub
     (global.get $global$0)
     (i32.const 16)
    )
   )
  )
  (block $block1
   (block $block6
    (block $block12
     (block $block26
      (block $block21
       (block $block24
        (block $block23
         (block $block13
          (block $block7
           (block $block2
            (if
             (i32.le_u
              (local.get $0)
              (i32.const 244)
             )
             (then
              (if
               (i32.and
                (local.tee $1
                 (i32.shr_u
                  (local.tee $4
                   (i32.load
                    (i32.const 3272)
                   )
                  )
                  (local.tee $0
                   (i32.shr_u
                    (local.tee $6
                     (select
                      (i32.const 16)
                      (i32.and
                       (i32.add
                        (local.get $0)
                        (i32.const 11)
                       )
                       (i32.const 504)
                      )
                      (i32.lt_u
                       (local.get $0)
                       (i32.const 11)
                      )
                     )
                    )
                    (i32.const 3)
                   )
                  )
                 )
                )
                (i32.const 3)
               )
               (then
                (block $block
                 (if
                  (i32.eq
                   (local.tee $0
                    (i32.add
                     (local.tee $1
                      (i32.shl
                       (local.tee $2
                        (i32.add
                         (i32.and
                          (i32.xor
                           (local.get $1)
                           (i32.const -1)
                          )
                          (i32.const 1)
                         )
                         (local.get $0)
                        )
                       )
                       (i32.const 3)
                      )
                     )
                     (i32.const 3312)
                    )
                   )
                   (local.tee $5
                    (i32.load offset=8
                     (local.tee $1
                      (i32.load
                       (i32.add
                        (local.get $1)
                        (i32.const 3320)
                       )
                      )
                     )
                    )
                   )
                  )
                  (then
                   (i32.store
                    (i32.const 3272)
                    (i32.and
                     (local.get $4)
                     (i32.rotl
                      (i32.const -2)
                      (local.get $2)
                     )
                    )
                   )
                   (br $block)
                  )
                 )
                 (i32.store offset=12
                  (local.get $5)
                  (local.get $0)
                 )
                 (i32.store offset=8
                  (local.get $0)
                  (local.get $5)
                 )
                )
                (local.set $0
                 (i32.add
                  (local.get $1)
                  (i32.const 8)
                 )
                )
                (i32.store offset=4
                 (local.get $1)
                 (i32.or
                  (local.tee $2
                   (i32.shl
                    (local.get $2)
                    (i32.const 3)
                   )
                  )
                  (i32.const 3)
                 )
                )
                (i32.store offset=4
                 (local.tee $1
                  (i32.add
                   (local.get $1)
                   (local.get $2)
                  )
                 )
                 (i32.or
                  (i32.load offset=4
                   (local.get $1)
                  )
                  (i32.const 1)
                 )
                )
                (br $block1)
               )
              )
              (br_if $block2
               (i32.le_u
                (local.get $6)
                (local.tee $8
                 (i32.load
                  (i32.const 3280)
                 )
                )
               )
              )
              (if
               (local.get $1)
               (then
                (block $block3
                 (if
                  (i32.eq
                   (local.tee $2
                    (i32.add
                     (local.tee $0
                      (i32.shl
                       (local.tee $1
                        (i32.ctz
                         (i32.and
                          (i32.or
                           (local.tee $2
                            (i32.shl
                             (i32.const 2)
                             (local.get $0)
                            )
                           )
                           (i32.sub
                            (i32.const 0)
                            (local.get $2)
                           )
                          )
                          (i32.shl
                           (local.get $1)
                           (local.get $0)
                          )
                         )
                        )
                       )
                       (i32.const 3)
                      )
                     )
                     (i32.const 3312)
                    )
                   )
                   (local.tee $5
                    (i32.load offset=8
                     (local.tee $0
                      (i32.load
                       (i32.add
                        (local.get $0)
                        (i32.const 3320)
                       )
                      )
                     )
                    )
                   )
                  )
                  (then
                   (i32.store
                    (i32.const 3272)
                    (local.tee $4
                     (i32.and
                      (local.get $4)
                      (i32.rotl
                       (i32.const -2)
                       (local.get $1)
                      )
                     )
                    )
                   )
                   (br $block3)
                  )
                 )
                 (i32.store offset=12
                  (local.get $5)
                  (local.get $2)
                 )
                 (i32.store offset=8
                  (local.get $2)
                  (local.get $5)
                 )
                )
                (i32.store offset=4
                 (local.get $0)
                 (i32.or
                  (local.get $6)
                  (i32.const 3)
                 )
                )
                (i32.store offset=4
                 (local.tee $7
                  (i32.add
                   (local.get $0)
                   (local.get $6)
                  )
                 )
                 (i32.or
                  (local.tee $5
                   (i32.sub
                    (local.tee $1
                     (i32.shl
                      (local.get $1)
                      (i32.const 3)
                     )
                    )
                    (local.get $6)
                   )
                  )
                  (i32.const 1)
                 )
                )
                (i32.store
                 (i32.add
                  (local.get $0)
                  (local.get $1)
                 )
                 (local.get $5)
                )
                (if
                 (local.get $8)
                 (then
                  (local.set $1
                   (i32.add
                    (i32.and
                     (local.get $8)
                     (i32.const -8)
                    )
                    (i32.const 3312)
                   )
                  )
                  (local.set $2
                   (i32.load
                    (i32.const 3292)
                   )
                  )
                  (local.set $3
                   (block $block4 (result i32)
                    (if
                     (i32.eqz
                      (i32.and
                       (local.get $4)
                       (local.tee $3
                        (i32.shl
                         (i32.const 1)
                         (i32.shr_u
                          (local.get $8)
                          (i32.const 3)
                         )
                        )
                       )
                      )
                     )
                     (then
                      (i32.store
                       (i32.const 3272)
                       (i32.or
                        (local.get $3)
                        (local.get $4)
                       )
                      )
                      (br $block4
                       (local.get $1)
                      )
                     )
                    )
                    (i32.load offset=8
                     (local.get $1)
                    )
                   )
                  )
                  (i32.store offset=8
                   (local.get $1)
                   (local.get $2)
                  )
                  (i32.store offset=12
                   (local.get $3)
                   (local.get $2)
                  )
                  (i32.store offset=12
                   (local.get $2)
                   (local.get $1)
                  )
                  (i32.store offset=8
                   (local.get $2)
                   (local.get $3)
                  )
                 )
                )
                (local.set $0
                 (i32.add
                  (local.get $0)
                  (i32.const 8)
                 )
                )
                (i32.store
                 (i32.const 3292)
                 (local.get $7)
                )
                (i32.store
                 (i32.const 3280)
                 (local.get $5)
                )
                (br $block1)
               )
              )
              (br_if $block2
               (i32.eqz
                (local.tee $11
                 (i32.load
                  (i32.const 3276)
                 )
                )
               )
              )
              (local.set $3
               (i32.sub
                (i32.and
                 (i32.load offset=4
                  (local.tee $2
                   (i32.load
                    (i32.add
                     (i32.shl
                      (i32.ctz
                       (local.get $11)
                      )
                      (i32.const 2)
                     )
                     (i32.const 3576)
                    )
                   )
                  )
                 )
                 (i32.const -8)
                )
                (local.get $6)
               )
              )
              (local.set $1
               (local.get $2)
              )
              (loop $label
               (block $block5
                (if
                 (i32.eqz
                  (local.tee $0
                   (i32.load offset=16
                    (local.get $1)
                   )
                  )
                 )
                 (then
                  (br_if $block5
                   (i32.eqz
                    (local.tee $0
                     (i32.load offset=20
                      (local.get $1)
                     )
                    )
                   )
                  )
                 )
                )
                (local.set $3
                 (select
                  (local.tee $1
                   (i32.sub
                    (i32.and
                     (i32.load offset=4
                      (local.get $0)
                     )
                     (i32.const -8)
                    )
                    (local.get $6)
                   )
                  )
                  (local.get $3)
                  (local.tee $1
                   (i32.lt_u
                    (local.get $1)
                    (local.get $3)
                   )
                  )
                 )
                )
                (local.set $2
                 (select
                  (local.get $0)
                  (local.get $2)
                  (local.get $1)
                 )
                )
                (local.set $1
                 (local.get $0)
                )
                (br $label)
               )
              )
              (local.set $9
               (i32.load offset=24
                (local.get $2)
               )
              )
              (if
               (i32.ne
                (local.get $2)
                (local.tee $0
                 (i32.load offset=12
                  (local.get $2)
                 )
                )
               )
               (then
                (i32.store offset=12
                 (local.tee $1
                  (i32.load offset=8
                   (local.get $2)
                  )
                 )
                 (local.get $0)
                )
                (i32.store offset=8
                 (local.get $0)
                 (local.get $1)
                )
                (br $block6)
               )
              )
              (local.set $5
               (if (result i32)
                (local.tee $1
                 (i32.load offset=20
                  (local.get $2)
                 )
                )
                (then
                 (i32.add
                  (local.get $2)
                  (i32.const 20)
                 )
                )
                (else
                 (br_if $block7
                  (i32.eqz
                   (local.tee $1
                    (i32.load offset=16
                     (local.get $2)
                    )
                   )
                  )
                 )
                 (i32.add
                  (local.get $2)
                  (i32.const 16)
                 )
                )
               )
              )
              (loop $label1
               (local.set $7
                (local.get $5)
               )
               (local.set $5
                (i32.add
                 (local.tee $0
                  (local.get $1)
                 )
                 (i32.const 20)
                )
               )
               (br_if $label1
                (local.tee $1
                 (i32.load offset=20
                  (local.get $0)
                 )
                )
               )
               (local.set $5
                (i32.add
                 (local.get $0)
                 (i32.const 16)
                )
               )
               (br_if $label1
                (local.tee $1
                 (i32.load offset=16
                  (local.get $0)
                 )
                )
               )
              )
              (i32.store
               (local.get $7)
               (i32.const 0)
              )
              (br $block6)
             )
            )
            (local.set $6
             (i32.const -1)
            )
            (br_if $block2
             (i32.gt_u
              (local.get $0)
              (i32.const -65)
             )
            )
            (local.set $6
             (i32.and
              (local.tee $1
               (i32.add
                (local.get $0)
                (i32.const 11)
               )
              )
              (i32.const -8)
             )
            )
            (br_if $block2
             (i32.eqz
              (local.tee $7
               (i32.load
                (i32.const 3276)
               )
              )
             )
            )
            (local.set $8
             (i32.const 31)
            )
            (local.set $3
             (i32.sub
              (i32.const 0)
              (local.get $6)
             )
            )
            (if
             (i32.le_u
              (local.get $0)
              (i32.const 16777204)
             )
             (then
              (local.set $8
               (i32.add
                (i32.sub
                 (i32.and
                  (i32.shr_u
                   (local.get $6)
                   (i32.sub
                    (i32.const 38)
                    (local.tee $0
                     (i32.clz
                      (i32.shr_u
                       (local.get $1)
                       (i32.const 8)
                      )
                     )
                    )
                   )
                  )
                  (i32.const 1)
                 )
                 (i32.shl
                  (local.get $0)
                  (i32.const 1)
                 )
                )
                (i32.const 62)
               )
              )
             )
            )
            (block $block11
             (block $block10
              (block $block8
               (if
                (i32.eqz
                 (local.tee $1
                  (i32.load
                   (i32.add
                    (i32.shl
                     (local.get $8)
                     (i32.const 2)
                    )
                    (i32.const 3576)
                   )
                  )
                 )
                )
                (then
                 (local.set $0
                  (i32.const 0)
                 )
                 (br $block8)
                )
               )
               (local.set $0
                (i32.const 0)
               )
               (local.set $2
                (i32.shl
                 (local.get $6)
                 (select
                  (i32.sub
                   (i32.const 25)
                   (i32.shr_u
                    (local.get $8)
                    (i32.const 1)
                   )
                  )
                  (i32.const 0)
                  (i32.ne
                   (local.get $8)
                   (i32.const 31)
                  )
                 )
                )
               )
               (loop $label2
                (block $block9
                 (br_if $block9
                  (i32.ge_u
                   (local.tee $4
                    (i32.sub
                     (i32.and
                      (i32.load offset=4
                       (local.get $1)
                      )
                      (i32.const -8)
                     )
                     (local.get $6)
                    )
                   )
                   (local.get $3)
                  )
                 )
                 (local.set $5
                  (local.get $1)
                 )
                 (br_if $block9
                  (local.tee $3
                   (local.get $4)
                  )
                 )
                 (local.set $3
                  (i32.const 0)
                 )
                 (local.set $0
                  (local.get $1)
                 )
                 (br $block10)
                )
                (local.set $0
                 (select
                  (select
                   (local.get $0)
                   (local.tee $4
                    (i32.load offset=20
                     (local.get $1)
                    )
                   )
                   (i32.eq
                    (local.get $4)
                    (local.tee $1
                     (i32.load offset=16
                      (i32.add
                       (local.get $1)
                       (i32.and
                        (i32.shr_u
                         (local.get $2)
                         (i32.const 29)
                        )
                        (i32.const 4)
                       )
                      )
                     )
                    )
                   )
                  )
                  (local.get $0)
                  (local.get $4)
                 )
                )
                (local.set $2
                 (i32.shl
                  (local.get $2)
                  (i32.const 1)
                 )
                )
                (br_if $label2
                 (local.get $1)
                )
               )
              )
              (if
               (i32.eqz
                (i32.or
                 (local.get $0)
                 (local.get $5)
                )
               )
               (then
                (local.set $5
                 (i32.const 0)
                )
                (br_if $block2
                 (i32.eqz
                  (local.tee $0
                   (i32.and
                    (i32.or
                     (local.tee $0
                      (i32.shl
                       (i32.const 2)
                       (local.get $8)
                      )
                     )
                     (i32.sub
                      (i32.const 0)
                      (local.get $0)
                     )
                    )
                    (local.get $7)
                   )
                  )
                 )
                )
                (local.set $0
                 (i32.load
                  (i32.add
                   (i32.shl
                    (i32.ctz
                     (local.get $0)
                    )
                    (i32.const 2)
                   )
                   (i32.const 3576)
                  )
                 )
                )
               )
              )
              (br_if $block11
               (i32.eqz
                (local.get $0)
               )
              )
             )
             (loop $label3
              (local.set $1
               (i32.lt_u
                (local.tee $2
                 (i32.sub
                  (i32.and
                   (i32.load offset=4
                    (local.get $0)
                   )
                   (i32.const -8)
                  )
                  (local.get $6)
                 )
                )
                (local.get $3)
               )
              )
              (local.set $3
               (select
                (local.get $2)
                (local.get $3)
                (local.get $1)
               )
              )
              (local.set $5
               (select
                (local.get $0)
                (local.get $5)
                (local.get $1)
               )
              )
              (br_if $label3
               (local.tee $0
                (if (result i32)
                 (local.tee $1
                  (i32.load offset=16
                   (local.get $0)
                  )
                 )
                 (then
                  (local.get $1)
                 )
                 (else
                  (i32.load offset=20
                   (local.get $0)
                  )
                 )
                )
               )
              )
             )
            )
            (br_if $block2
             (i32.eqz
              (local.get $5)
             )
            )
            (br_if $block2
             (i32.ge_u
              (local.get $3)
              (i32.sub
               (i32.load
                (i32.const 3280)
               )
               (local.get $6)
              )
             )
            )
            (local.set $8
             (i32.load offset=24
              (local.get $5)
             )
            )
            (if
             (i32.ne
              (local.get $5)
              (local.tee $0
               (i32.load offset=12
                (local.get $5)
               )
              )
             )
             (then
              (i32.store offset=12
               (local.tee $1
                (i32.load offset=8
                 (local.get $5)
                )
               )
               (local.get $0)
              )
              (i32.store offset=8
               (local.get $0)
               (local.get $1)
              )
              (br $block12)
             )
            )
            (local.set $2
             (if (result i32)
              (local.tee $1
               (i32.load offset=20
                (local.get $5)
               )
              )
              (then
               (i32.add
                (local.get $5)
                (i32.const 20)
               )
              )
              (else
               (br_if $block13
                (i32.eqz
                 (local.tee $1
                  (i32.load offset=16
                   (local.get $5)
                  )
                 )
                )
               )
               (i32.add
                (local.get $5)
                (i32.const 16)
               )
              )
             )
            )
            (loop $label4
             (local.set $4
              (local.get $2)
             )
             (local.set $2
              (i32.add
               (local.tee $0
                (local.get $1)
               )
               (i32.const 20)
              )
             )
             (br_if $label4
              (local.tee $1
               (i32.load offset=20
                (local.get $0)
               )
              )
             )
             (local.set $2
              (i32.add
               (local.get $0)
               (i32.const 16)
              )
             )
             (br_if $label4
              (local.tee $1
               (i32.load offset=16
                (local.get $0)
               )
              )
             )
            )
            (i32.store
             (local.get $4)
             (i32.const 0)
            )
            (br $block12)
           )
           (if
            (i32.le_u
             (local.get $6)
             (local.tee $5
              (i32.load
               (i32.const 3280)
              )
             )
            )
            (then
             (local.set $0
              (i32.load
               (i32.const 3292)
              )
             )
             (block $block14
              (if
               (i32.ge_u
                (local.tee $1
                 (i32.sub
                  (local.get $5)
                  (local.get $6)
                 )
                )
                (i32.const 16)
               )
               (then
                (i32.store offset=4
                 (local.tee $2
                  (i32.add
                   (local.get $0)
                   (local.get $6)
                  )
                 )
                 (i32.or
                  (local.get $1)
                  (i32.const 1)
                 )
                )
                (i32.store
                 (i32.add
                  (local.get $0)
                  (local.get $5)
                 )
                 (local.get $1)
                )
                (i32.store offset=4
                 (local.get $0)
                 (i32.or
                  (local.get $6)
                  (i32.const 3)
                 )
                )
                (br $block14)
               )
              )
              (i32.store offset=4
               (local.get $0)
               (i32.or
                (local.get $5)
                (i32.const 3)
               )
              )
              (i32.store offset=4
               (local.tee $1
                (i32.add
                 (local.get $0)
                 (local.get $5)
                )
               )
               (i32.or
                (i32.load offset=4
                 (local.get $1)
                )
                (i32.const 1)
               )
              )
              (local.set $2
               (i32.const 0)
              )
              (local.set $1
               (i32.const 0)
              )
             )
             (i32.store
              (i32.const 3280)
              (local.get $1)
             )
             (i32.store
              (i32.const 3292)
              (local.get $2)
             )
             (local.set $0
              (i32.add
               (local.get $0)
               (i32.const 8)
              )
             )
             (br $block1)
            )
           )
           (if
            (i32.lt_u
             (local.get $6)
             (local.tee $2
              (i32.load
               (i32.const 3284)
              )
             )
            )
            (then
             (i32.store
              (i32.const 3284)
              (local.tee $1
               (i32.sub
                (local.get $2)
                (local.get $6)
               )
              )
             )
             (i32.store
              (i32.const 3296)
              (local.tee $2
               (i32.add
                (local.tee $0
                 (i32.load
                  (i32.const 3296)
                 )
                )
                (local.get $6)
               )
              )
             )
             (i32.store offset=4
              (local.get $2)
              (i32.or
               (local.get $1)
               (i32.const 1)
              )
             )
             (i32.store offset=4
              (local.get $0)
              (i32.or
               (local.get $6)
               (i32.const 3)
              )
             )
             (local.set $0
              (i32.add
               (local.get $0)
               (i32.const 8)
              )
             )
             (br $block1)
            )
           )
           (local.set $0
            (i32.const 0)
           )
           (br_if $block1
            (i32.le_u
             (local.tee $1
              (i32.and
               (local.tee $4
                (i32.add
                 (local.tee $3
                  (i32.add
                   (local.get $6)
                   (i32.const 47)
                  )
                 )
                 (local.tee $1
                  (block $block15 (result i32)
                   (if
                    (i32.load
                     (i32.const 3744)
                    )
                    (then
                     (br $block15
                      (i32.load
                       (i32.const 3752)
                      )
                     )
                    )
                   )
                   (i64.store align=4
                    (i32.const 3756)
                    (i64.const -1)
                   )
                   (i64.store align=4
                    (i32.const 3748)
                    (i64.const 17592186048512)
                   )
                   (i32.store
                    (i32.const 3744)
                    (i32.xor
                     (i32.and
                      (i32.add
                       (local.get $10)
                       (i32.const 12)
                      )
                      (i32.const -16)
                     )
                     (i32.const 1431655768)
                    )
                   )
                   (i32.store
                    (i32.const 3764)
                    (i32.const 0)
                   )
                   (i32.store
                    (i32.const 3716)
                    (i32.const 0)
                   )
                   (i32.const 4096)
                  )
                 )
                )
               )
               (local.tee $7
                (i32.sub
                 (i32.const 0)
                 (local.get $1)
                )
               )
              )
             )
             (local.get $6)
            )
           )
           (if
            (local.tee $5
             (i32.load
              (i32.const 3712)
             )
            )
            (then
             (br_if $block1
              (i32.le_u
               (local.tee $9
                (i32.add
                 (local.tee $8
                  (i32.load
                   (i32.const 3704)
                  )
                 )
                 (local.get $1)
                )
               )
               (local.get $8)
              )
             )
             (br_if $block1
              (i32.lt_u
               (local.get $5)
               (local.get $9)
              )
             )
            )
           )
           (block $block19
            (if
             (i32.eqz
              (i32.and
               (i32.load8_u
                (i32.const 3716)
               )
               (i32.const 4)
              )
             )
             (then
              (block $block17
               (block $block20
                (block $block18
                 (block $block16
                  (if
                   (local.tee $5
                    (i32.load
                     (i32.const 3296)
                    )
                   )
                   (then
                    (local.set $0
                     (i32.const 3720)
                    )
                    (loop $label5
                     (if
                      (i32.le_u
                       (local.tee $8
                        (i32.load
                         (local.get $0)
                        )
                       )
                       (local.get $5)
                      )
                      (then
                       (br_if $block16
                        (i32.lt_u
                         (local.get $5)
                         (i32.add
                          (local.get $8)
                          (i32.load offset=4
                           (local.get $0)
                          )
                         )
                        )
                       )
                      )
                     )
                     (br_if $label5
                      (local.tee $0
                       (i32.load offset=8
                        (local.get $0)
                       )
                      )
                     )
                    )
                   )
                  )
                  (br_if $block17
                   (i32.eq
                    (local.tee $2
                     (call $23
                      (i32.const 0)
                     )
                    )
                    (i32.const -1)
                   )
                  )
                  (local.set $4
                   (local.get $1)
                  )
                  (if
                   (i32.and
                    (local.tee $5
                     (i32.sub
                      (local.tee $0
                       (i32.load
                        (i32.const 3748)
                       )
                      )
                      (i32.const 1)
                     )
                    )
                    (local.get $2)
                   )
                   (then
                    (local.set $4
                     (i32.add
                      (i32.sub
                       (local.get $1)
                       (local.get $2)
                      )
                      (i32.and
                       (i32.add
                        (local.get $2)
                        (local.get $5)
                       )
                       (i32.sub
                        (i32.const 0)
                        (local.get $0)
                       )
                      )
                     )
                    )
                   )
                  )
                  (br_if $block17
                   (i32.le_u
                    (local.get $4)
                    (local.get $6)
                   )
                  )
                  (if
                   (local.tee $0
                    (i32.load
                     (i32.const 3712)
                    )
                   )
                   (then
                    (br_if $block17
                     (i32.le_u
                      (local.tee $7
                       (i32.add
                        (local.tee $5
                         (i32.load
                          (i32.const 3704)
                         )
                        )
                        (local.get $4)
                       )
                      )
                      (local.get $5)
                     )
                    )
                    (br_if $block17
                     (i32.lt_u
                      (local.get $0)
                      (local.get $7)
                     )
                    )
                   )
                  )
                  (br_if $block18
                   (i32.ne
                    (local.tee $0
                     (call $23
                      (local.get $4)
                     )
                    )
                    (local.get $2)
                   )
                  )
                  (br $block19)
                 )
                 (br_if $block20
                  (i32.eq
                   (local.tee $2
                    (call $23
                     (local.tee $4
                      (i32.and
                       (i32.sub
                        (local.get $4)
                        (local.get $2)
                       )
                       (local.get $7)
                      )
                     )
                    )
                   )
                   (i32.add
                    (i32.load
                     (local.get $0)
                    )
                    (i32.load offset=4
                     (local.get $0)
                    )
                   )
                  )
                 )
                 (local.set $0
                  (local.get $2)
                 )
                )
                (br_if $block17
                 (i32.eq
                  (local.get $0)
                  (i32.const -1)
                 )
                )
                (if
                 (i32.le_u
                  (i32.add
                   (local.get $6)
                   (i32.const 48)
                  )
                  (local.get $4)
                 )
                 (then
                  (local.set $2
                   (local.get $0)
                  )
                  (br $block19)
                 )
                )
                (br_if $block17
                 (i32.eq
                  (call $23
                   (local.tee $2
                    (i32.and
                     (i32.add
                      (local.tee $2
                       (i32.load
                        (i32.const 3752)
                       )
                      )
                      (i32.sub
                       (local.get $3)
                       (local.get $4)
                      )
                     )
                     (i32.sub
                      (i32.const 0)
                      (local.get $2)
                     )
                    )
                   )
                  )
                  (i32.const -1)
                 )
                )
                (local.set $4
                 (i32.add
                  (local.get $2)
                  (local.get $4)
                 )
                )
                (local.set $2
                 (local.get $0)
                )
                (br $block19)
               )
               (br_if $block19
                (i32.ne
                 (local.get $2)
                 (i32.const -1)
                )
               )
              )
              (i32.store
               (i32.const 3716)
               (i32.or
                (i32.load
                 (i32.const 3716)
                )
                (i32.const 4)
               )
              )
             )
            )
            (local.set $2
             (call $23
              (local.get $1)
             )
            )
            (local.set $0
             (call $23
              (i32.const 0)
             )
            )
            (br_if $block21
             (i32.eq
              (local.get $2)
              (i32.const -1)
             )
            )
            (br_if $block21
             (i32.eq
              (local.get $0)
              (i32.const -1)
             )
            )
            (br_if $block21
             (i32.le_u
              (local.get $0)
              (local.get $2)
             )
            )
            (br_if $block21
             (i32.le_u
              (local.tee $4
               (i32.sub
                (local.get $0)
                (local.get $2)
               )
              )
              (i32.add
               (local.get $6)
               (i32.const 40)
              )
             )
            )
           )
           (i32.store
            (i32.const 3704)
            (local.tee $0
             (i32.add
              (i32.load
               (i32.const 3704)
              )
              (local.get $4)
             )
            )
           )
           (if
            (i32.lt_u
             (i32.load
              (i32.const 3708)
             )
             (local.get $0)
            )
            (then
             (i32.store
              (i32.const 3708)
              (local.get $0)
             )
            )
           )
           (block $block22
            (if
             (local.tee $3
              (i32.load
               (i32.const 3296)
              )
             )
             (then
              (local.set $0
               (i32.const 3720)
              )
              (loop $label6
               (br_if $block22
                (i32.eq
                 (local.get $2)
                 (i32.add
                  (local.tee $1
                   (i32.load
                    (local.get $0)
                   )
                  )
                  (local.tee $5
                   (i32.load offset=4
                    (local.get $0)
                   )
                  )
                 )
                )
               )
               (br_if $label6
                (local.tee $0
                 (i32.load offset=8
                  (local.get $0)
                 )
                )
               )
              )
              (br $block23)
             )
            )
            (if
             (i32.eqz
              (select
               (local.tee $0
                (i32.load
                 (i32.const 3288)
                )
               )
               (i32.const 0)
               (i32.le_u
                (local.get $0)
                (local.get $2)
               )
              )
             )
             (then
              (i32.store
               (i32.const 3288)
               (local.get $2)
              )
             )
            )
            (local.set $0
             (i32.const 0)
            )
            (i32.store
             (i32.const 3724)
             (local.get $4)
            )
            (i32.store
             (i32.const 3720)
             (local.get $2)
            )
            (i32.store
             (i32.const 3304)
             (i32.const -1)
            )
            (i32.store
             (i32.const 3308)
             (i32.load
              (i32.const 3744)
             )
            )
            (i32.store
             (i32.const 3732)
             (i32.const 0)
            )
            (loop $label7
             (i32.store
              (i32.add
               (local.tee $1
                (i32.shl
                 (local.get $0)
                 (i32.const 3)
                )
               )
               (i32.const 3320)
              )
              (local.tee $5
               (i32.add
                (local.get $1)
                (i32.const 3312)
               )
              )
             )
             (i32.store
              (i32.add
               (local.get $1)
               (i32.const 3324)
              )
              (local.get $5)
             )
             (br_if $label7
              (i32.ne
               (local.tee $0
                (i32.add
                 (local.get $0)
                 (i32.const 1)
                )
               )
               (i32.const 32)
              )
             )
            )
            (i32.store
             (i32.const 3284)
             (local.tee $5
              (i32.sub
               (local.tee $0
                (i32.sub
                 (local.get $4)
                 (i32.const 40)
                )
               )
               (local.tee $1
                (i32.and
                 (i32.sub
                  (i32.const -8)
                  (local.get $2)
                 )
                 (i32.const 7)
                )
               )
              )
             )
            )
            (i32.store
             (i32.const 3296)
             (local.tee $1
              (i32.add
               (local.get $1)
               (local.get $2)
              )
             )
            )
            (i32.store offset=4
             (local.get $1)
             (i32.or
              (local.get $5)
              (i32.const 1)
             )
            )
            (i32.store offset=4
             (i32.add
              (local.get $0)
              (local.get $2)
             )
             (i32.const 40)
            )
            (i32.store
             (i32.const 3300)
             (i32.load
              (i32.const 3760)
             )
            )
            (br $block24)
           )
           (br_if $block23
            (i32.le_u
             (local.get $2)
             (local.get $3)
            )
           )
           (br_if $block23
            (i32.gt_u
             (local.get $1)
             (local.get $3)
            )
           )
           (br_if $block23
            (i32.and
             (i32.load offset=12
              (local.get $0)
             )
             (i32.const 8)
            )
           )
           (i32.store offset=4
            (local.get $0)
            (i32.add
             (local.get $4)
             (local.get $5)
            )
           )
           (i32.store
            (i32.const 3296)
            (local.tee $1
             (i32.add
              (local.get $3)
              (local.tee $0
               (i32.and
                (i32.sub
                 (i32.const -8)
                 (local.get $3)
                )
                (i32.const 7)
               )
              )
             )
            )
           )
           (i32.store
            (i32.const 3284)
            (local.tee $0
             (i32.sub
              (local.tee $2
               (i32.add
                (i32.load
                 (i32.const 3284)
                )
                (local.get $4)
               )
              )
              (local.get $0)
             )
            )
           )
           (i32.store offset=4
            (local.get $1)
            (i32.or
             (local.get $0)
             (i32.const 1)
            )
           )
           (i32.store offset=4
            (i32.add
             (local.get $2)
             (local.get $3)
            )
            (i32.const 40)
           )
           (i32.store
            (i32.const 3300)
            (i32.load
             (i32.const 3760)
            )
           )
           (br $block24)
          )
          (local.set $0
           (i32.const 0)
          )
          (br $block6)
         )
         (local.set $0
          (i32.const 0)
         )
         (br $block12)
        )
        (if
         (i32.gt_u
          (i32.load
           (i32.const 3288)
          )
          (local.get $2)
         )
         (then
          (i32.store
           (i32.const 3288)
           (local.get $2)
          )
         )
        )
        (local.set $5
         (i32.add
          (local.get $2)
          (local.get $4)
         )
        )
        (local.set $0
         (i32.const 3720)
        )
        (block $block25
         (loop $label8
          (if
           (i32.ne
            (local.get $5)
            (local.tee $1
             (i32.load
              (local.get $0)
             )
            )
           )
           (then
            (br_if $label8
             (local.tee $0
              (i32.load offset=8
               (local.get $0)
              )
             )
            )
            (br $block25)
           )
          )
         )
         (br_if $block26
          (i32.eqz
           (i32.and
            (i32.load8_u offset=12
             (local.get $0)
            )
            (i32.const 8)
           )
          )
         )
        )
        (local.set $0
         (i32.const 3720)
        )
        (loop $label9
         (block $block27
          (if
           (i32.le_u
            (local.tee $1
             (i32.load
              (local.get $0)
             )
            )
            (local.get $3)
           )
           (then
            (br_if $block27
             (i32.lt_u
              (local.get $3)
              (local.tee $5
               (i32.add
                (local.get $1)
                (i32.load offset=4
                 (local.get $0)
                )
               )
              )
             )
            )
           )
          )
          (local.set $0
           (i32.load offset=8
            (local.get $0)
           )
          )
          (br $label9)
         )
        )
        (i32.store
         (i32.const 3284)
         (local.tee $7
          (i32.sub
           (local.tee $0
            (i32.sub
             (local.get $4)
             (i32.const 40)
            )
           )
           (local.tee $1
            (i32.and
             (i32.sub
              (i32.const -8)
              (local.get $2)
             )
             (i32.const 7)
            )
           )
          )
         )
        )
        (i32.store
         (i32.const 3296)
         (local.tee $1
          (i32.add
           (local.get $1)
           (local.get $2)
          )
         )
        )
        (i32.store offset=4
         (local.get $1)
         (i32.or
          (local.get $7)
          (i32.const 1)
         )
        )
        (i32.store offset=4
         (i32.add
          (local.get $0)
          (local.get $2)
         )
         (i32.const 40)
        )
        (i32.store
         (i32.const 3300)
         (i32.load
          (i32.const 3760)
         )
        )
        (i32.store offset=4
         (local.tee $1
          (select
           (local.get $3)
           (local.tee $0
            (i32.sub
             (i32.add
              (local.get $5)
              (i32.and
               (i32.sub
                (i32.const 39)
                (local.get $5)
               )
               (i32.const 7)
              )
             )
             (i32.const 47)
            )
           )
           (i32.lt_u
            (local.get $0)
            (i32.add
             (local.get $3)
             (i32.const 16)
            )
           )
          )
         )
         (i32.const 27)
        )
        (i64.store offset=16 align=4
         (local.get $1)
         (i64.load align=4
          (i32.const 3728)
         )
        )
        (i64.store offset=8 align=4
         (local.get $1)
         (i64.load align=4
          (i32.const 3720)
         )
        )
        (i32.store
         (i32.const 3728)
         (i32.add
          (local.get $1)
          (i32.const 8)
         )
        )
        (i32.store
         (i32.const 3724)
         (local.get $4)
        )
        (i32.store
         (i32.const 3720)
         (local.get $2)
        )
        (i32.store
         (i32.const 3732)
         (i32.const 0)
        )
        (local.set $0
         (i32.add
          (local.get $1)
          (i32.const 24)
         )
        )
        (loop $label10
         (i32.store offset=4
          (local.get $0)
          (i32.const 7)
         )
         (local.set $2
          (i32.add
           (local.get $0)
           (i32.const 8)
          )
         )
         (local.set $0
          (i32.add
           (local.get $0)
           (i32.const 4)
          )
         )
         (br_if $label10
          (i32.lt_u
           (local.get $2)
           (local.get $5)
          )
         )
        )
        (br_if $block24
         (i32.eq
          (local.get $1)
          (local.get $3)
         )
        )
        (i32.store offset=4
         (local.get $1)
         (i32.and
          (i32.load offset=4
           (local.get $1)
          )
          (i32.const -2)
         )
        )
        (i32.store offset=4
         (local.get $3)
         (i32.or
          (local.tee $2
           (i32.sub
            (local.get $1)
            (local.get $3)
           )
          )
          (i32.const 1)
         )
        )
        (i32.store
         (local.get $1)
         (local.get $2)
        )
        (i32.store
         (i32.add
          (block $block29 (result i32)
           (if
            (i32.le_u
             (local.get $2)
             (i32.const 255)
            )
            (then
             (local.set $0
              (i32.add
               (i32.and
                (local.get $2)
                (i32.const -8)
               )
               (i32.const 3312)
              )
             )
             (local.set $1
              (block $block28 (result i32)
               (if
                (i32.eqz
                 (i32.and
                  (local.tee $1
                   (i32.load
                    (i32.const 3272)
                   )
                  )
                  (local.tee $2
                   (i32.shl
                    (i32.const 1)
                    (i32.shr_u
                     (local.get $2)
                     (i32.const 3)
                    )
                   )
                  )
                 )
                )
                (then
                 (i32.store
                  (i32.const 3272)
                  (i32.or
                   (local.get $1)
                   (local.get $2)
                  )
                 )
                 (br $block28
                  (local.get $0)
                 )
                )
               )
               (i32.load offset=8
                (local.get $0)
               )
              )
             )
             (i32.store offset=8
              (local.get $0)
              (local.get $3)
             )
             (i32.store offset=12
              (local.get $1)
              (local.get $3)
             )
             (local.set $2
              (i32.const 12)
             )
             (br $block29
              (i32.const 8)
             )
            )
           )
           (local.set $0
            (i32.const 31)
           )
           (if
            (i32.le_u
             (local.get $2)
             (i32.const 16777215)
            )
            (then
             (local.set $0
              (i32.add
               (i32.sub
                (i32.and
                 (i32.shr_u
                  (local.get $2)
                  (i32.sub
                   (i32.const 38)
                   (local.tee $0
                    (i32.clz
                     (i32.shr_u
                      (local.get $2)
                      (i32.const 8)
                     )
                    )
                   )
                  )
                 )
                 (i32.const 1)
                )
                (i32.shl
                 (local.get $0)
                 (i32.const 1)
                )
               )
               (i32.const 62)
              )
             )
            )
           )
           (i32.store offset=28
            (local.get $3)
            (local.get $0)
           )
           (i64.store offset=16 align=4
            (local.get $3)
            (i64.const 0)
           )
           (local.set $1
            (i32.add
             (i32.shl
              (local.get $0)
              (i32.const 2)
             )
             (i32.const 3576)
            )
           )
           (block $block31
            (block $block30
             (if
              (i32.eqz
               (i32.and
                (local.tee $5
                 (i32.load
                  (i32.const 3276)
                 )
                )
                (local.tee $4
                 (i32.shl
                  (i32.const 1)
                  (local.get $0)
                 )
                )
               )
              )
              (then
               (i32.store
                (i32.const 3276)
                (i32.or
                 (local.get $4)
                 (local.get $5)
                )
               )
               (i32.store
                (local.get $1)
                (local.get $3)
               )
               (br $block30)
              )
             )
             (local.set $0
              (i32.shl
               (local.get $2)
               (select
                (i32.sub
                 (i32.const 25)
                 (i32.shr_u
                  (local.get $0)
                  (i32.const 1)
                 )
                )
                (i32.const 0)
                (i32.ne
                 (local.get $0)
                 (i32.const 31)
                )
               )
              )
             )
             (local.set $5
              (i32.load
               (local.get $1)
              )
             )
             (loop $label11
              (br_if $block31
               (i32.eq
                (i32.and
                 (i32.load offset=4
                  (local.tee $1
                   (local.get $5)
                  )
                 )
                 (i32.const -8)
                )
                (local.get $2)
               )
              )
              (local.set $5
               (i32.shr_u
                (local.get $0)
                (i32.const 29)
               )
              )
              (local.set $0
               (i32.shl
                (local.get $0)
                (i32.const 1)
               )
              )
              (br_if $label11
               (local.tee $5
                (i32.load offset=16
                 (local.tee $4
                  (i32.add
                   (local.get $1)
                   (i32.and
                    (local.get $5)
                    (i32.const 4)
                   )
                  )
                 )
                )
               )
              )
             )
             (i32.store offset=16
              (local.get $4)
              (local.get $3)
             )
            )
            (i32.store offset=24
             (local.get $3)
             (local.get $1)
            )
            (local.set $2
             (i32.const 8)
            )
            (local.set $0
             (local.tee $1
              (local.get $3)
             )
            )
            (br $block29
             (i32.const 12)
            )
           )
           (i32.store offset=12
            (local.tee $0
             (i32.load offset=8
              (local.get $1)
             )
            )
            (local.get $3)
           )
           (i32.store offset=8
            (local.get $1)
            (local.get $3)
           )
           (i32.store offset=8
            (local.get $3)
            (local.get $0)
           )
           (local.set $0
            (i32.const 0)
           )
           (local.set $2
            (i32.const 24)
           )
           (i32.const 12)
          )
          (local.get $3)
         )
         (local.get $1)
        )
        (i32.store
         (i32.add
          (local.get $2)
          (local.get $3)
         )
         (local.get $0)
        )
       )
       (br_if $block21
        (i32.le_u
         (local.tee $0
          (i32.load
           (i32.const 3284)
          )
         )
         (local.get $6)
        )
       )
       (i32.store
        (i32.const 3284)
        (local.tee $1
         (i32.sub
          (local.get $0)
          (local.get $6)
         )
        )
       )
       (i32.store
        (i32.const 3296)
        (local.tee $2
         (i32.add
          (local.tee $0
           (i32.load
            (i32.const 3296)
           )
          )
          (local.get $6)
         )
        )
       )
       (i32.store offset=4
        (local.get $2)
        (i32.or
         (local.get $1)
         (i32.const 1)
        )
       )
       (i32.store offset=4
        (local.get $0)
        (i32.or
         (local.get $6)
         (i32.const 3)
        )
       )
       (local.set $0
        (i32.add
         (local.get $0)
         (i32.const 8)
        )
       )
       (br $block1)
      )
      (i32.store
       (i32.const 2220)
       (i32.const 48)
      )
      (local.set $0
       (i32.const 0)
      )
      (br $block1)
     )
     (i32.store
      (local.get $0)
      (local.get $2)
     )
     (i32.store offset=4
      (local.get $0)
      (i32.add
       (i32.load offset=4
        (local.get $0)
       )
       (local.get $4)
      )
     )
     (i32.store offset=4
      (local.tee $8
       (i32.add
        (local.get $2)
        (i32.and
         (i32.sub
          (i32.const -8)
          (local.get $2)
         )
         (i32.const 7)
        )
       )
      )
      (i32.or
       (local.get $6)
       (i32.const 3)
      )
     )
     (local.set $7
      (i32.sub
       (local.tee $4
        (i32.add
         (local.get $1)
         (i32.and
          (i32.sub
           (i32.const -8)
           (local.get $1)
          )
          (i32.const 7)
         )
        )
       )
       (local.tee $3
        (i32.add
         (local.get $6)
         (local.get $8)
        )
       )
      )
     )
     (block $block32
      (if
       (i32.eq
        (i32.load
         (i32.const 3296)
        )
        (local.get $4)
       )
       (then
        (i32.store
         (i32.const 3296)
         (local.get $3)
        )
        (i32.store
         (i32.const 3284)
         (local.tee $0
          (i32.add
           (i32.load
            (i32.const 3284)
           )
           (local.get $7)
          )
         )
        )
        (i32.store offset=4
         (local.get $3)
         (i32.or
          (local.get $0)
          (i32.const 1)
         )
        )
        (br $block32)
       )
      )
      (if
       (i32.eq
        (i32.load
         (i32.const 3292)
        )
        (local.get $4)
       )
       (then
        (i32.store
         (i32.const 3292)
         (local.get $3)
        )
        (i32.store
         (i32.const 3280)
         (local.tee $0
          (i32.add
           (i32.load
            (i32.const 3280)
           )
           (local.get $7)
          )
         )
        )
        (i32.store offset=4
         (local.get $3)
         (i32.or
          (local.get $0)
          (i32.const 1)
         )
        )
        (i32.store
         (i32.add
          (local.get $0)
          (local.get $3)
         )
         (local.get $0)
        )
        (br $block32)
       )
      )
      (if
       (i32.eq
        (i32.and
         (local.tee $0
          (i32.load offset=4
           (local.get $4)
          )
         )
         (i32.const 3)
        )
        (i32.const 1)
       )
       (then
        (local.set $9
         (i32.and
          (local.get $0)
          (i32.const -8)
         )
        )
        (local.set $2
         (i32.load offset=12
          (local.get $4)
         )
        )
        (block $block33
         (if
          (i32.le_u
           (local.get $0)
           (i32.const 255)
          )
          (then
           (if
            (i32.eq
             (local.tee $1
              (i32.load offset=8
               (local.get $4)
              )
             )
             (local.get $2)
            )
            (then
             (i32.store
              (i32.const 3272)
              (i32.and
               (i32.load
                (i32.const 3272)
               )
               (i32.rotl
                (i32.const -2)
                (i32.shr_u
                 (local.get $0)
                 (i32.const 3)
                )
               )
              )
             )
             (br $block33)
            )
           )
           (i32.store offset=12
            (local.get $1)
            (local.get $2)
           )
           (i32.store offset=8
            (local.get $2)
            (local.get $1)
           )
           (br $block33)
          )
         )
         (local.set $6
          (i32.load offset=24
           (local.get $4)
          )
         )
         (block $block34
          (if
           (i32.ne
            (local.get $2)
            (local.get $4)
           )
           (then
            (i32.store offset=12
             (local.tee $0
              (i32.load offset=8
               (local.get $4)
              )
             )
             (local.get $2)
            )
            (i32.store offset=8
             (local.get $2)
             (local.get $0)
            )
            (br $block34)
           )
          )
          (block $block35
           (local.set $1
            (if (result i32)
             (local.tee $0
              (i32.load offset=20
               (local.get $4)
              )
             )
             (then
              (i32.add
               (local.get $4)
               (i32.const 20)
              )
             )
             (else
              (br_if $block35
               (i32.eqz
                (local.tee $0
                 (i32.load offset=16
                  (local.get $4)
                 )
                )
               )
              )
              (i32.add
               (local.get $4)
               (i32.const 16)
              )
             )
            )
           )
           (loop $label12
            (local.set $5
             (local.get $1)
            )
            (local.set $1
             (i32.add
              (local.tee $2
               (local.get $0)
              )
              (i32.const 20)
             )
            )
            (br_if $label12
             (local.tee $0
              (i32.load offset=20
               (local.get $0)
              )
             )
            )
            (local.set $1
             (i32.add
              (local.get $2)
              (i32.const 16)
             )
            )
            (br_if $label12
             (local.tee $0
              (i32.load offset=16
               (local.get $2)
              )
             )
            )
           )
           (i32.store
            (local.get $5)
            (i32.const 0)
           )
           (br $block34)
          )
          (local.set $2
           (i32.const 0)
          )
         )
         (br_if $block33
          (i32.eqz
           (local.get $6)
          )
         )
         (block $block36
          (if
           (i32.eq
            (i32.load
             (local.tee $1
              (i32.add
               (i32.shl
                (local.tee $0
                 (i32.load offset=28
                  (local.get $4)
                 )
                )
                (i32.const 2)
               )
               (i32.const 3576)
              )
             )
            )
            (local.get $4)
           )
           (then
            (i32.store
             (local.get $1)
             (local.get $2)
            )
            (br_if $block36
             (local.get $2)
            )
            (i32.store
             (i32.const 3276)
             (i32.and
              (i32.load
               (i32.const 3276)
              )
              (i32.rotl
               (i32.const -2)
               (local.get $0)
              )
             )
            )
            (br $block33)
           )
          )
          (block $block37
           (if
            (i32.eq
             (local.get $4)
             (i32.load offset=16
              (local.get $6)
             )
            )
            (then
             (i32.store offset=16
              (local.get $6)
              (local.get $2)
             )
             (br $block37)
            )
           )
           (i32.store offset=20
            (local.get $6)
            (local.get $2)
           )
          )
          (br_if $block33
           (i32.eqz
            (local.get $2)
           )
          )
         )
         (i32.store offset=24
          (local.get $2)
          (local.get $6)
         )
         (if
          (local.tee $0
           (i32.load offset=16
            (local.get $4)
           )
          )
          (then
           (i32.store offset=16
            (local.get $2)
            (local.get $0)
           )
           (i32.store offset=24
            (local.get $0)
            (local.get $2)
           )
          )
         )
         (br_if $block33
          (i32.eqz
           (local.tee $0
            (i32.load offset=20
             (local.get $4)
            )
           )
          )
         )
         (i32.store offset=20
          (local.get $2)
          (local.get $0)
         )
         (i32.store offset=24
          (local.get $0)
          (local.get $2)
         )
        )
        (local.set $7
         (i32.add
          (local.get $7)
          (local.get $9)
         )
        )
        (local.set $0
         (i32.load offset=4
          (local.tee $4
           (i32.add
            (local.get $4)
            (local.get $9)
           )
          )
         )
        )
       )
      )
      (i32.store offset=4
       (local.get $4)
       (i32.and
        (local.get $0)
        (i32.const -2)
       )
      )
      (i32.store offset=4
       (local.get $3)
       (i32.or
        (local.get $7)
        (i32.const 1)
       )
      )
      (i32.store
       (i32.add
        (local.get $3)
        (local.get $7)
       )
       (local.get $7)
      )
      (if
       (i32.le_u
        (local.get $7)
        (i32.const 255)
       )
       (then
        (local.set $0
         (i32.add
          (i32.and
           (local.get $7)
           (i32.const -8)
          )
          (i32.const 3312)
         )
        )
        (local.set $1
         (block $block38 (result i32)
          (if
           (i32.eqz
            (i32.and
             (local.tee $1
              (i32.load
               (i32.const 3272)
              )
             )
             (local.tee $2
              (i32.shl
               (i32.const 1)
               (i32.shr_u
                (local.get $7)
                (i32.const 3)
               )
              )
             )
            )
           )
           (then
            (i32.store
             (i32.const 3272)
             (i32.or
              (local.get $1)
              (local.get $2)
             )
            )
            (br $block38
             (local.get $0)
            )
           )
          )
          (i32.load offset=8
           (local.get $0)
          )
         )
        )
        (i32.store offset=8
         (local.get $0)
         (local.get $3)
        )
        (i32.store offset=12
         (local.get $1)
         (local.get $3)
        )
        (i32.store offset=12
         (local.get $3)
         (local.get $0)
        )
        (i32.store offset=8
         (local.get $3)
         (local.get $1)
        )
        (br $block32)
       )
      )
      (local.set $2
       (i32.const 31)
      )
      (if
       (i32.le_u
        (local.get $7)
        (i32.const 16777215)
       )
       (then
        (local.set $2
         (i32.add
          (i32.sub
           (i32.and
            (i32.shr_u
             (local.get $7)
             (i32.sub
              (i32.const 38)
              (local.tee $0
               (i32.clz
                (i32.shr_u
                 (local.get $7)
                 (i32.const 8)
                )
               )
              )
             )
            )
            (i32.const 1)
           )
           (i32.shl
            (local.get $0)
            (i32.const 1)
           )
          )
          (i32.const 62)
         )
        )
       )
      )
      (i32.store offset=28
       (local.get $3)
       (local.get $2)
      )
      (i64.store offset=16 align=4
       (local.get $3)
       (i64.const 0)
      )
      (local.set $0
       (i32.add
        (i32.shl
         (local.get $2)
         (i32.const 2)
        )
        (i32.const 3576)
       )
      )
      (block $block40
       (block $block39
        (if
         (i32.eqz
          (i32.and
           (local.tee $1
            (i32.load
             (i32.const 3276)
            )
           )
           (local.tee $5
            (i32.shl
             (i32.const 1)
             (local.get $2)
            )
           )
          )
         )
         (then
          (i32.store
           (i32.const 3276)
           (i32.or
            (local.get $1)
            (local.get $5)
           )
          )
          (i32.store
           (local.get $0)
           (local.get $3)
          )
          (br $block39)
         )
        )
        (local.set $2
         (i32.shl
          (local.get $7)
          (select
           (i32.sub
            (i32.const 25)
            (i32.shr_u
             (local.get $2)
             (i32.const 1)
            )
           )
           (i32.const 0)
           (i32.ne
            (local.get $2)
            (i32.const 31)
           )
          )
         )
        )
        (local.set $1
         (i32.load
          (local.get $0)
         )
        )
        (loop $label13
         (br_if $block40
          (i32.eq
           (i32.and
            (i32.load offset=4
             (local.tee $0
              (local.get $1)
             )
            )
            (i32.const -8)
           )
           (local.get $7)
          )
         )
         (local.set $1
          (i32.shr_u
           (local.get $2)
           (i32.const 29)
          )
         )
         (local.set $2
          (i32.shl
           (local.get $2)
           (i32.const 1)
          )
         )
         (br_if $label13
          (local.tee $1
           (i32.load offset=16
            (local.tee $5
             (i32.add
              (local.get $0)
              (i32.and
               (local.get $1)
               (i32.const 4)
              )
             )
            )
           )
          )
         )
        )
        (i32.store offset=16
         (local.get $5)
         (local.get $3)
        )
       )
       (i32.store offset=24
        (local.get $3)
        (local.get $0)
       )
       (i32.store offset=12
        (local.get $3)
        (local.get $3)
       )
       (i32.store offset=8
        (local.get $3)
        (local.get $3)
       )
       (br $block32)
      )
      (i32.store offset=12
       (local.tee $1
        (i32.load offset=8
         (local.get $0)
        )
       )
       (local.get $3)
      )
      (i32.store offset=8
       (local.get $0)
       (local.get $3)
      )
      (i32.store offset=24
       (local.get $3)
       (i32.const 0)
      )
      (i32.store offset=12
       (local.get $3)
       (local.get $0)
      )
      (i32.store offset=8
       (local.get $3)
       (local.get $1)
      )
     )
     (local.set $0
      (i32.add
       (local.get $8)
       (i32.const 8)
      )
     )
     (br $block1)
    )
    (block $block41
     (br_if $block41
      (i32.eqz
       (local.get $8)
      )
     )
     (block $block42
      (if
       (i32.eq
        (i32.load
         (local.tee $2
          (i32.add
           (i32.shl
            (local.tee $1
             (i32.load offset=28
              (local.get $5)
             )
            )
            (i32.const 2)
           )
           (i32.const 3576)
          )
         )
        )
        (local.get $5)
       )
       (then
        (i32.store
         (local.get $2)
         (local.get $0)
        )
        (br_if $block42
         (local.get $0)
        )
        (i32.store
         (i32.const 3276)
         (local.tee $7
          (i32.and
           (local.get $7)
           (i32.rotl
            (i32.const -2)
            (local.get $1)
           )
          )
         )
        )
        (br $block41)
       )
      )
      (block $block43
       (if
        (i32.eq
         (local.get $5)
         (i32.load offset=16
          (local.get $8)
         )
        )
        (then
         (i32.store offset=16
          (local.get $8)
          (local.get $0)
         )
         (br $block43)
        )
       )
       (i32.store offset=20
        (local.get $8)
        (local.get $0)
       )
      )
      (br_if $block41
       (i32.eqz
        (local.get $0)
       )
      )
     )
     (i32.store offset=24
      (local.get $0)
      (local.get $8)
     )
     (if
      (local.tee $1
       (i32.load offset=16
        (local.get $5)
       )
      )
      (then
       (i32.store offset=16
        (local.get $0)
        (local.get $1)
       )
       (i32.store offset=24
        (local.get $1)
        (local.get $0)
       )
      )
     )
     (br_if $block41
      (i32.eqz
       (local.tee $1
        (i32.load offset=20
         (local.get $5)
        )
       )
      )
     )
     (i32.store offset=20
      (local.get $0)
      (local.get $1)
     )
     (i32.store offset=24
      (local.get $1)
      (local.get $0)
     )
    )
    (block $block44
     (if
      (i32.le_u
       (local.get $3)
       (i32.const 15)
      )
      (then
       (i32.store offset=4
        (local.get $5)
        (i32.or
         (local.tee $0
          (i32.add
           (local.get $3)
           (local.get $6)
          )
         )
         (i32.const 3)
        )
       )
       (i32.store offset=4
        (local.tee $0
         (i32.add
          (local.get $0)
          (local.get $5)
         )
        )
        (i32.or
         (i32.load offset=4
          (local.get $0)
         )
         (i32.const 1)
        )
       )
       (br $block44)
      )
     )
     (i32.store offset=4
      (local.get $5)
      (i32.or
       (local.get $6)
       (i32.const 3)
      )
     )
     (i32.store offset=4
      (local.tee $4
       (i32.add
        (local.get $5)
        (local.get $6)
       )
      )
      (i32.or
       (local.get $3)
       (i32.const 1)
      )
     )
     (i32.store
      (i32.add
       (local.get $3)
       (local.get $4)
      )
      (local.get $3)
     )
     (if
      (i32.le_u
       (local.get $3)
       (i32.const 255)
      )
      (then
       (local.set $0
        (i32.add
         (i32.and
          (local.get $3)
          (i32.const -8)
         )
         (i32.const 3312)
        )
       )
       (local.set $1
        (block $block45 (result i32)
         (if
          (i32.eqz
           (i32.and
            (local.tee $1
             (i32.load
              (i32.const 3272)
             )
            )
            (local.tee $2
             (i32.shl
              (i32.const 1)
              (i32.shr_u
               (local.get $3)
               (i32.const 3)
              )
             )
            )
           )
          )
          (then
           (i32.store
            (i32.const 3272)
            (i32.or
             (local.get $1)
             (local.get $2)
            )
           )
           (br $block45
            (local.get $0)
           )
          )
         )
         (i32.load offset=8
          (local.get $0)
         )
        )
       )
       (i32.store offset=8
        (local.get $0)
        (local.get $4)
       )
       (i32.store offset=12
        (local.get $1)
        (local.get $4)
       )
       (i32.store offset=12
        (local.get $4)
        (local.get $0)
       )
       (i32.store offset=8
        (local.get $4)
        (local.get $1)
       )
       (br $block44)
      )
     )
     (local.set $0
      (i32.const 31)
     )
     (if
      (i32.le_u
       (local.get $3)
       (i32.const 16777215)
      )
      (then
       (local.set $0
        (i32.add
         (i32.sub
          (i32.and
           (i32.shr_u
            (local.get $3)
            (i32.sub
             (i32.const 38)
             (local.tee $0
              (i32.clz
               (i32.shr_u
                (local.get $3)
                (i32.const 8)
               )
              )
             )
            )
           )
           (i32.const 1)
          )
          (i32.shl
           (local.get $0)
           (i32.const 1)
          )
         )
         (i32.const 62)
        )
       )
      )
     )
     (i32.store offset=28
      (local.get $4)
      (local.get $0)
     )
     (i64.store offset=16 align=4
      (local.get $4)
      (i64.const 0)
     )
     (local.set $1
      (i32.add
       (i32.shl
        (local.get $0)
        (i32.const 2)
       )
       (i32.const 3576)
      )
     )
     (block $block47
      (block $block46
       (if
        (i32.eqz
         (i32.and
          (local.get $7)
          (local.tee $2
           (i32.shl
            (i32.const 1)
            (local.get $0)
           )
          )
         )
        )
        (then
         (i32.store
          (i32.const 3276)
          (i32.or
           (local.get $2)
           (local.get $7)
          )
         )
         (i32.store
          (local.get $1)
          (local.get $4)
         )
         (i32.store offset=24
          (local.get $4)
          (local.get $1)
         )
         (br $block46)
        )
       )
       (local.set $0
        (i32.shl
         (local.get $3)
         (select
          (i32.sub
           (i32.const 25)
           (i32.shr_u
            (local.get $0)
            (i32.const 1)
           )
          )
          (i32.const 0)
          (i32.ne
           (local.get $0)
           (i32.const 31)
          )
         )
        )
       )
       (local.set $1
        (i32.load
         (local.get $1)
        )
       )
       (loop $label14
        (br_if $block47
         (i32.eq
          (i32.and
           (i32.load offset=4
            (local.tee $2
             (local.get $1)
            )
           )
           (i32.const -8)
          )
          (local.get $3)
         )
        )
        (local.set $1
         (i32.shr_u
          (local.get $0)
          (i32.const 29)
         )
        )
        (local.set $0
         (i32.shl
          (local.get $0)
          (i32.const 1)
         )
        )
        (br_if $label14
         (local.tee $1
          (i32.load offset=16
           (local.tee $7
            (i32.add
             (local.get $2)
             (i32.and
              (local.get $1)
              (i32.const 4)
             )
            )
           )
          )
         )
        )
       )
       (i32.store offset=16
        (local.get $7)
        (local.get $4)
       )
       (i32.store offset=24
        (local.get $4)
        (local.get $2)
       )
      )
      (i32.store offset=12
       (local.get $4)
       (local.get $4)
      )
      (i32.store offset=8
       (local.get $4)
       (local.get $4)
      )
      (br $block44)
     )
     (i32.store offset=12
      (local.tee $0
       (i32.load offset=8
        (local.get $2)
       )
      )
      (local.get $4)
     )
     (i32.store offset=8
      (local.get $2)
      (local.get $4)
     )
     (i32.store offset=24
      (local.get $4)
      (i32.const 0)
     )
     (i32.store offset=12
      (local.get $4)
      (local.get $2)
     )
     (i32.store offset=8
      (local.get $4)
      (local.get $0)
     )
    )
    (local.set $0
     (i32.add
      (local.get $5)
      (i32.const 8)
     )
    )
    (br $block1)
   )
   (block $block48
    (br_if $block48
     (i32.eqz
      (local.get $9)
     )
    )
    (block $block49
     (if
      (i32.eq
       (i32.load
        (local.tee $5
         (i32.add
          (i32.shl
           (local.tee $1
            (i32.load offset=28
             (local.get $2)
            )
           )
           (i32.const 2)
          )
          (i32.const 3576)
         )
        )
       )
       (local.get $2)
      )
      (then
       (i32.store
        (local.get $5)
        (local.get $0)
       )
       (br_if $block49
        (local.get $0)
       )
       (i32.store
        (i32.const 3276)
        (i32.and
         (local.get $11)
         (i32.rotl
          (i32.const -2)
          (local.get $1)
         )
        )
       )
       (br $block48)
      )
     )
     (block $block50
      (if
       (i32.eq
        (local.get $2)
        (i32.load offset=16
         (local.get $9)
        )
       )
       (then
        (i32.store offset=16
         (local.get $9)
         (local.get $0)
        )
        (br $block50)
       )
      )
      (i32.store offset=20
       (local.get $9)
       (local.get $0)
      )
     )
     (br_if $block48
      (i32.eqz
       (local.get $0)
      )
     )
    )
    (i32.store offset=24
     (local.get $0)
     (local.get $9)
    )
    (if
     (local.tee $1
      (i32.load offset=16
       (local.get $2)
      )
     )
     (then
      (i32.store offset=16
       (local.get $0)
       (local.get $1)
      )
      (i32.store offset=24
       (local.get $1)
       (local.get $0)
      )
     )
    )
    (br_if $block48
     (i32.eqz
      (local.tee $1
       (i32.load offset=20
        (local.get $2)
       )
      )
     )
    )
    (i32.store offset=20
     (local.get $0)
     (local.get $1)
    )
    (i32.store offset=24
     (local.get $1)
     (local.get $0)
    )
   )
   (block $block51
    (if
     (i32.le_u
      (local.get $3)
      (i32.const 15)
     )
     (then
      (i32.store offset=4
       (local.get $2)
       (i32.or
        (local.tee $0
         (i32.add
          (local.get $3)
          (local.get $6)
         )
        )
        (i32.const 3)
       )
      )
      (i32.store offset=4
       (local.tee $0
        (i32.add
         (local.get $0)
         (local.get $2)
        )
       )
       (i32.or
        (i32.load offset=4
         (local.get $0)
        )
        (i32.const 1)
       )
      )
      (br $block51)
     )
    )
    (i32.store offset=4
     (local.get $2)
     (i32.or
      (local.get $6)
      (i32.const 3)
     )
    )
    (i32.store offset=4
     (local.tee $5
      (i32.add
       (local.get $2)
       (local.get $6)
      )
     )
     (i32.or
      (local.get $3)
      (i32.const 1)
     )
    )
    (i32.store
     (i32.add
      (local.get $3)
      (local.get $5)
     )
     (local.get $3)
    )
    (if
     (local.get $8)
     (then
      (local.set $0
       (i32.add
        (i32.and
         (local.get $8)
         (i32.const -8)
        )
        (i32.const 3312)
       )
      )
      (local.set $1
       (i32.load
        (i32.const 3292)
       )
      )
      (local.set $4
       (block $block52 (result i32)
        (if
         (i32.eqz
          (i32.and
           (local.tee $7
            (i32.shl
             (i32.const 1)
             (i32.shr_u
              (local.get $8)
              (i32.const 3)
             )
            )
           )
           (local.get $4)
          )
         )
         (then
          (i32.store
           (i32.const 3272)
           (i32.or
            (local.get $4)
            (local.get $7)
           )
          )
          (br $block52
           (local.get $0)
          )
         )
        )
        (i32.load offset=8
         (local.get $0)
        )
       )
      )
      (i32.store offset=8
       (local.get $0)
       (local.get $1)
      )
      (i32.store offset=12
       (local.get $4)
       (local.get $1)
      )
      (i32.store offset=12
       (local.get $1)
       (local.get $0)
      )
      (i32.store offset=8
       (local.get $1)
       (local.get $4)
      )
     )
    )
    (i32.store
     (i32.const 3292)
     (local.get $5)
    )
    (i32.store
     (i32.const 3280)
     (local.get $3)
    )
   )
   (local.set $0
    (i32.add
     (local.get $2)
     (i32.const 8)
    )
   )
  )
  (global.set $global$0
   (i32.add
    (local.get $10)
    (i32.const 16)
   )
  )
  (local.get $0)
 )
 (func $20 (param $0 i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (block $block
   (br_if $block
    (i32.eqz
     (local.get $0)
    )
   )
   (local.set $5
    (i32.add
     (local.tee $3
      (i32.sub
       (local.get $0)
       (i32.const 8)
      )
     )
     (local.tee $0
      (i32.and
       (local.tee $2
        (i32.load
         (i32.sub
          (local.get $0)
          (i32.const 4)
         )
        )
       )
       (i32.const -8)
      )
     )
    )
   )
   (block $block1
    (br_if $block1
     (i32.and
      (local.get $2)
      (i32.const 1)
     )
    )
    (br_if $block
     (i32.eqz
      (i32.and
       (local.get $2)
       (i32.const 2)
      )
     )
    )
    (br_if $block
     (i32.lt_u
      (local.tee $3
       (i32.sub
        (local.get $3)
        (local.tee $4
         (i32.load
          (local.get $3)
         )
        )
       )
      )
      (i32.load
       (i32.const 3288)
      )
     )
    )
    (local.set $0
     (i32.add
      (local.get $0)
      (local.get $4)
     )
    )
    (block $block3
     (block $block4
      (block $block2
       (if
        (i32.ne
         (i32.load
          (i32.const 3292)
         )
         (local.get $3)
        )
        (then
         (local.set $1
          (i32.load offset=12
           (local.get $3)
          )
         )
         (if
          (i32.le_u
           (local.get $4)
           (i32.const 255)
          )
          (then
           (br_if $block2
            (i32.ne
             (local.get $1)
             (local.tee $2
              (i32.load offset=8
               (local.get $3)
              )
             )
            )
           )
           (i32.store
            (i32.const 3272)
            (i32.and
             (i32.load
              (i32.const 3272)
             )
             (i32.rotl
              (i32.const -2)
              (i32.shr_u
               (local.get $4)
               (i32.const 3)
              )
             )
            )
           )
           (br $block1)
          )
         )
         (local.set $7
          (i32.load offset=24
           (local.get $3)
          )
         )
         (if
          (i32.ne
           (local.get $1)
           (local.get $3)
          )
          (then
           (i32.store offset=12
            (local.tee $2
             (i32.load offset=8
              (local.get $3)
             )
            )
            (local.get $1)
           )
           (i32.store offset=8
            (local.get $1)
            (local.get $2)
           )
           (br $block3)
          )
         )
         (local.set $4
          (if (result i32)
           (local.tee $2
            (i32.load offset=20
             (local.get $3)
            )
           )
           (then
            (i32.add
             (local.get $3)
             (i32.const 20)
            )
           )
           (else
            (br_if $block4
             (i32.eqz
              (local.tee $2
               (i32.load offset=16
                (local.get $3)
               )
              )
             )
            )
            (i32.add
             (local.get $3)
             (i32.const 16)
            )
           )
          )
         )
         (loop $label
          (local.set $6
           (local.get $4)
          )
          (local.set $4
           (i32.add
            (local.tee $1
             (local.get $2)
            )
            (i32.const 20)
           )
          )
          (br_if $label
           (local.tee $2
            (i32.load offset=20
             (local.get $1)
            )
           )
          )
          (local.set $4
           (i32.add
            (local.get $1)
            (i32.const 16)
           )
          )
          (br_if $label
           (local.tee $2
            (i32.load offset=16
             (local.get $1)
            )
           )
          )
         )
         (i32.store
          (local.get $6)
          (i32.const 0)
         )
         (br $block3)
        )
       )
       (br_if $block1
        (i32.ne
         (i32.and
          (local.tee $2
           (i32.load offset=4
            (local.get $5)
           )
          )
          (i32.const 3)
         )
         (i32.const 3)
        )
       )
       (i32.store
        (i32.const 3280)
        (local.get $0)
       )
       (i32.store offset=4
        (local.get $5)
        (i32.and
         (local.get $2)
         (i32.const -2)
        )
       )
       (i32.store offset=4
        (local.get $3)
        (i32.or
         (local.get $0)
         (i32.const 1)
        )
       )
       (i32.store
        (local.get $5)
        (local.get $0)
       )
       (return)
      )
      (i32.store offset=12
       (local.get $2)
       (local.get $1)
      )
      (i32.store offset=8
       (local.get $1)
       (local.get $2)
      )
      (br $block1)
     )
     (local.set $1
      (i32.const 0)
     )
    )
    (br_if $block1
     (i32.eqz
      (local.get $7)
     )
    )
    (block $block5
     (if
      (i32.eq
       (i32.load
        (local.tee $2
         (i32.add
          (i32.shl
           (local.tee $4
            (i32.load offset=28
             (local.get $3)
            )
           )
           (i32.const 2)
          )
          (i32.const 3576)
         )
        )
       )
       (local.get $3)
      )
      (then
       (i32.store
        (local.get $2)
        (local.get $1)
       )
       (br_if $block5
        (local.get $1)
       )
       (i32.store
        (i32.const 3276)
        (i32.and
         (i32.load
          (i32.const 3276)
         )
         (i32.rotl
          (i32.const -2)
          (local.get $4)
         )
        )
       )
       (br $block1)
      )
     )
     (block $block6
      (if
       (i32.eq
        (local.get $3)
        (i32.load offset=16
         (local.get $7)
        )
       )
       (then
        (i32.store offset=16
         (local.get $7)
         (local.get $1)
        )
        (br $block6)
       )
      )
      (i32.store offset=20
       (local.get $7)
       (local.get $1)
      )
     )
     (br_if $block1
      (i32.eqz
       (local.get $1)
      )
     )
    )
    (i32.store offset=24
     (local.get $1)
     (local.get $7)
    )
    (if
     (local.tee $2
      (i32.load offset=16
       (local.get $3)
      )
     )
     (then
      (i32.store offset=16
       (local.get $1)
       (local.get $2)
      )
      (i32.store offset=24
       (local.get $2)
       (local.get $1)
      )
     )
    )
    (br_if $block1
     (i32.eqz
      (local.tee $2
       (i32.load offset=20
        (local.get $3)
       )
      )
     )
    )
    (i32.store offset=20
     (local.get $1)
     (local.get $2)
    )
    (i32.store offset=24
     (local.get $2)
     (local.get $1)
    )
   )
   (br_if $block
    (i32.ge_u
     (local.get $3)
     (local.get $5)
    )
   )
   (br_if $block
    (i32.eqz
     (i32.and
      (local.tee $4
       (i32.load offset=4
        (local.get $5)
       )
      )
      (i32.const 1)
     )
    )
   )
   (block $block10
    (block $block7
     (block $block8
      (block $block9
       (if
        (i32.eqz
         (i32.and
          (local.get $4)
          (i32.const 2)
         )
        )
        (then
         (if
          (i32.eq
           (i32.load
            (i32.const 3296)
           )
           (local.get $5)
          )
          (then
           (i32.store
            (i32.const 3296)
            (local.get $3)
           )
           (i32.store
            (i32.const 3284)
            (local.tee $0
             (i32.add
              (i32.load
               (i32.const 3284)
              )
              (local.get $0)
             )
            )
           )
           (i32.store offset=4
            (local.get $3)
            (i32.or
             (local.get $0)
             (i32.const 1)
            )
           )
           (br_if $block
            (i32.ne
             (local.get $3)
             (i32.load
              (i32.const 3292)
             )
            )
           )
           (i32.store
            (i32.const 3280)
            (i32.const 0)
           )
           (i32.store
            (i32.const 3292)
            (i32.const 0)
           )
           (return)
          )
         )
         (if
          (i32.eq
           (local.tee $7
            (i32.load
             (i32.const 3292)
            )
           )
           (local.get $5)
          )
          (then
           (i32.store
            (i32.const 3292)
            (local.get $3)
           )
           (i32.store
            (i32.const 3280)
            (local.tee $0
             (i32.add
              (i32.load
               (i32.const 3280)
              )
              (local.get $0)
             )
            )
           )
           (i32.store offset=4
            (local.get $3)
            (i32.or
             (local.get $0)
             (i32.const 1)
            )
           )
           (i32.store
            (i32.add
             (local.get $0)
             (local.get $3)
            )
            (local.get $0)
           )
           (return)
          )
         )
         (local.set $0
          (i32.add
           (i32.and
            (local.get $4)
            (i32.const -8)
           )
           (local.get $0)
          )
         )
         (local.set $1
          (i32.load offset=12
           (local.get $5)
          )
         )
         (if
          (i32.le_u
           (local.get $4)
           (i32.const 255)
          )
          (then
           (if
            (i32.eq
             (local.tee $2
              (i32.load offset=8
               (local.get $5)
              )
             )
             (local.get $1)
            )
            (then
             (i32.store
              (i32.const 3272)
              (i32.and
               (i32.load
                (i32.const 3272)
               )
               (i32.rotl
                (i32.const -2)
                (i32.shr_u
                 (local.get $4)
                 (i32.const 3)
                )
               )
              )
             )
             (br $block7)
            )
           )
           (i32.store offset=12
            (local.get $2)
            (local.get $1)
           )
           (i32.store offset=8
            (local.get $1)
            (local.get $2)
           )
           (br $block7)
          )
         )
         (local.set $8
          (i32.load offset=24
           (local.get $5)
          )
         )
         (if
          (i32.ne
           (local.get $1)
           (local.get $5)
          )
          (then
           (i32.store offset=12
            (local.tee $2
             (i32.load offset=8
              (local.get $5)
             )
            )
            (local.get $1)
           )
           (i32.store offset=8
            (local.get $1)
            (local.get $2)
           )
           (br $block8)
          )
         )
         (local.set $4
          (if (result i32)
           (local.tee $2
            (i32.load offset=20
             (local.get $5)
            )
           )
           (then
            (i32.add
             (local.get $5)
             (i32.const 20)
            )
           )
           (else
            (br_if $block9
             (i32.eqz
              (local.tee $2
               (i32.load offset=16
                (local.get $5)
               )
              )
             )
            )
            (i32.add
             (local.get $5)
             (i32.const 16)
            )
           )
          )
         )
         (loop $label1
          (local.set $6
           (local.get $4)
          )
          (local.set $4
           (i32.add
            (local.tee $1
             (local.get $2)
            )
            (i32.const 20)
           )
          )
          (br_if $label1
           (local.tee $2
            (i32.load offset=20
             (local.get $1)
            )
           )
          )
          (local.set $4
           (i32.add
            (local.get $1)
            (i32.const 16)
           )
          )
          (br_if $label1
           (local.tee $2
            (i32.load offset=16
             (local.get $1)
            )
           )
          )
         )
         (i32.store
          (local.get $6)
          (i32.const 0)
         )
         (br $block8)
        )
       )
       (i32.store offset=4
        (local.get $5)
        (i32.and
         (local.get $4)
         (i32.const -2)
        )
       )
       (i32.store offset=4
        (local.get $3)
        (i32.or
         (local.get $0)
         (i32.const 1)
        )
       )
       (i32.store
        (i32.add
         (local.get $0)
         (local.get $3)
        )
        (local.get $0)
       )
       (br $block10)
      )
      (local.set $1
       (i32.const 0)
      )
     )
     (br_if $block7
      (i32.eqz
       (local.get $8)
      )
     )
     (block $block11
      (if
       (i32.eq
        (i32.load
         (local.tee $2
          (i32.add
           (i32.shl
            (local.tee $4
             (i32.load offset=28
              (local.get $5)
             )
            )
            (i32.const 2)
           )
           (i32.const 3576)
          )
         )
        )
        (local.get $5)
       )
       (then
        (i32.store
         (local.get $2)
         (local.get $1)
        )
        (br_if $block11
         (local.get $1)
        )
        (i32.store
         (i32.const 3276)
         (i32.and
          (i32.load
           (i32.const 3276)
          )
          (i32.rotl
           (i32.const -2)
           (local.get $4)
          )
         )
        )
        (br $block7)
       )
      )
      (block $block12
       (if
        (i32.eq
         (local.get $5)
         (i32.load offset=16
          (local.get $8)
         )
        )
        (then
         (i32.store offset=16
          (local.get $8)
          (local.get $1)
         )
         (br $block12)
        )
       )
       (i32.store offset=20
        (local.get $8)
        (local.get $1)
       )
      )
      (br_if $block7
       (i32.eqz
        (local.get $1)
       )
      )
     )
     (i32.store offset=24
      (local.get $1)
      (local.get $8)
     )
     (if
      (local.tee $2
       (i32.load offset=16
        (local.get $5)
       )
      )
      (then
       (i32.store offset=16
        (local.get $1)
        (local.get $2)
       )
       (i32.store offset=24
        (local.get $2)
        (local.get $1)
       )
      )
     )
     (br_if $block7
      (i32.eqz
       (local.tee $2
        (i32.load offset=20
         (local.get $5)
        )
       )
      )
     )
     (i32.store offset=20
      (local.get $1)
      (local.get $2)
     )
     (i32.store offset=24
      (local.get $2)
      (local.get $1)
     )
    )
    (i32.store offset=4
     (local.get $3)
     (i32.or
      (local.get $0)
      (i32.const 1)
     )
    )
    (i32.store
     (i32.add
      (local.get $0)
      (local.get $3)
     )
     (local.get $0)
    )
    (br_if $block10
     (i32.ne
      (local.get $3)
      (local.get $7)
     )
    )
    (i32.store
     (i32.const 3280)
     (local.get $0)
    )
    (return)
   )
   (if
    (i32.le_u
     (local.get $0)
     (i32.const 255)
    )
    (then
     (local.set $2
      (i32.add
       (i32.and
        (local.get $0)
        (i32.const -8)
       )
       (i32.const 3312)
      )
     )
     (local.set $0
      (block $block13 (result i32)
       (if
        (i32.eqz
         (i32.and
          (local.tee $4
           (i32.load
            (i32.const 3272)
           )
          )
          (local.tee $0
           (i32.shl
            (i32.const 1)
            (i32.shr_u
             (local.get $0)
             (i32.const 3)
            )
           )
          )
         )
        )
        (then
         (i32.store
          (i32.const 3272)
          (i32.or
           (local.get $0)
           (local.get $4)
          )
         )
         (br $block13
          (local.get $2)
         )
        )
       )
       (i32.load offset=8
        (local.get $2)
       )
      )
     )
     (i32.store offset=8
      (local.get $2)
      (local.get $3)
     )
     (i32.store offset=12
      (local.get $0)
      (local.get $3)
     )
     (i32.store offset=12
      (local.get $3)
      (local.get $2)
     )
     (i32.store offset=8
      (local.get $3)
      (local.get $0)
     )
     (return)
    )
   )
   (local.set $1
    (i32.const 31)
   )
   (if
    (i32.le_u
     (local.get $0)
     (i32.const 16777215)
    )
    (then
     (local.set $1
      (i32.add
       (i32.sub
        (i32.and
         (i32.shr_u
          (local.get $0)
          (i32.sub
           (i32.const 38)
           (local.tee $2
            (i32.clz
             (i32.shr_u
              (local.get $0)
              (i32.const 8)
             )
            )
           )
          )
         )
         (i32.const 1)
        )
        (i32.shl
         (local.get $2)
         (i32.const 1)
        )
       )
       (i32.const 62)
      )
     )
    )
   )
   (i32.store offset=28
    (local.get $3)
    (local.get $1)
   )
   (i64.store offset=16 align=4
    (local.get $3)
    (i64.const 0)
   )
   (local.set $4
    (i32.add
     (i32.shl
      (local.get $1)
      (i32.const 2)
     )
     (i32.const 3576)
    )
   )
   (local.set $6
    (block $block16 (result i32)
     (block $block15
      (local.set $0
       (block $block14 (result i32)
        (if
         (i32.eqz
          (i32.and
           (local.tee $6
            (i32.load
             (i32.const 3276)
            )
           )
           (local.tee $2
            (i32.shl
             (i32.const 1)
             (local.get $1)
            )
           )
          )
         )
         (then
          (i32.store
           (i32.const 3276)
           (i32.or
            (local.get $2)
            (local.get $6)
           )
          )
          (i32.store
           (local.get $4)
           (local.get $3)
          )
          (local.set $1
           (i32.const 24)
          )
          (br $block14
           (i32.const 8)
          )
         )
        )
        (local.set $1
         (i32.shl
          (local.get $0)
          (select
           (i32.sub
            (i32.const 25)
            (i32.shr_u
             (local.get $1)
             (i32.const 1)
            )
           )
           (i32.const 0)
           (i32.ne
            (local.get $1)
            (i32.const 31)
           )
          )
         )
        )
        (local.set $4
         (i32.load
          (local.get $4)
         )
        )
        (loop $label2
         (br_if $block15
          (i32.eq
           (i32.and
            (i32.load offset=4
             (local.tee $2
              (local.get $4)
             )
            )
            (i32.const -8)
           )
           (local.get $0)
          )
         )
         (local.set $4
          (i32.shr_u
           (local.get $1)
           (i32.const 29)
          )
         )
         (local.set $1
          (i32.shl
           (local.get $1)
           (i32.const 1)
          )
         )
         (br_if $label2
          (local.tee $4
           (i32.load offset=16
            (local.tee $6
             (i32.add
              (local.get $2)
              (i32.and
               (local.get $4)
               (i32.const 4)
              )
             )
            )
           )
          )
         )
        )
        (i32.store offset=16
         (local.get $6)
         (local.get $3)
        )
        (local.set $1
         (i32.const 24)
        )
        (local.set $4
         (local.get $2)
        )
        (i32.const 8)
       )
      )
      (br $block16
       (local.tee $2
        (local.get $3)
       )
      )
     )
     (i32.store offset=12
      (local.tee $4
       (i32.load offset=8
        (local.get $2)
       )
      )
      (local.get $3)
     )
     (i32.store offset=8
      (local.get $2)
      (local.get $3)
     )
     (local.set $0
      (i32.const 24)
     )
     (local.set $1
      (i32.const 8)
     )
     (i32.const 0)
    )
   )
   (i32.store
    (i32.add
     (local.get $1)
     (local.get $3)
    )
    (local.get $4)
   )
   (i32.store offset=12
    (local.get $3)
    (local.get $2)
   )
   (i32.store
    (i32.add
     (local.get $0)
     (local.get $3)
    )
    (local.get $6)
   )
   (i32.store
    (i32.const 3304)
    (select
     (local.tee $0
      (i32.sub
       (i32.load
        (i32.const 3304)
       )
       (i32.const 1)
      )
     )
     (i32.const -1)
     (local.get $0)
    )
   )
  )
 )
 (func $21 (param $0 i32) (param $1 i32) (result i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local $9 i32)
  (local $10 i32)
  (local $11 i32)
  (local $12 i32)
  (if
   (i32.eqz
    (local.get $0)
   )
   (then
    (return
     (call $19
      (local.get $1)
     )
    )
   )
  )
  (if
   (i32.ge_u
    (local.get $1)
    (i32.const -64)
   )
   (then
    (i32.store
     (i32.const 2220)
     (i32.const 48)
    )
    (return
     (i32.const 0)
    )
   )
  )
  (if
   (local.tee $2
    (block $block1 (result i32)
     (local.set $6
      (select
       (i32.const 16)
       (i32.and
        (i32.add
         (local.get $1)
         (i32.const 11)
        )
        (i32.const -8)
       )
       (i32.lt_u
        (local.get $1)
        (i32.const 11)
       )
      )
     )
     (local.set $8
      (i32.and
       (local.tee $9
        (i32.load offset=4
         (local.tee $4
          (i32.sub
           (local.get $0)
           (i32.const 8)
          )
         )
        )
       )
       (i32.const -8)
      )
     )
     (block $block
      (if
       (i32.eqz
        (i32.and
         (local.get $9)
         (i32.const 3)
        )
       )
       (then
        (br_if $block
         (i32.lt_u
          (local.get $6)
          (i32.const 256)
         )
        )
        (if
         (i32.le_u
          (i32.add
           (local.get $6)
           (i32.const 4)
          )
          (local.get $8)
         )
         (then
          (local.set $2
           (local.get $4)
          )
          (br_if $block
           (i32.le_u
            (i32.sub
             (local.get $8)
             (local.get $6)
            )
            (i32.shl
             (i32.load
              (i32.const 3752)
             )
             (i32.const 1)
            )
           )
          )
         )
        )
        (br $block1
         (i32.const 0)
        )
       )
      )
      (local.set $7
       (i32.add
        (local.get $4)
        (local.get $8)
       )
      )
      (block $block2
       (if
        (i32.le_u
         (local.get $6)
         (local.get $8)
        )
        (then
         (br_if $block2
          (i32.lt_u
           (local.tee $3
            (i32.sub
             (local.get $8)
             (local.get $6)
            )
           )
           (i32.const 16)
          )
         )
         (i32.store offset=4
          (local.get $4)
          (i32.or
           (i32.or
            (local.get $6)
            (i32.and
             (local.get $9)
             (i32.const 1)
            )
           )
           (i32.const 2)
          )
         )
         (i32.store offset=4
          (local.tee $2
           (i32.add
            (local.get $4)
            (local.get $6)
           )
          )
          (i32.or
           (local.get $3)
           (i32.const 3)
          )
         )
         (i32.store offset=4
          (local.get $7)
          (i32.or
           (i32.load offset=4
            (local.get $7)
           )
           (i32.const 1)
          )
         )
         (call $22
          (local.get $2)
          (local.get $3)
         )
         (br $block2)
        )
       )
       (if
        (i32.eq
         (i32.load
          (i32.const 3296)
         )
         (local.get $7)
        )
        (then
         (br_if $block
          (i32.le_u
           (local.tee $8
            (i32.add
             (i32.load
              (i32.const 3284)
             )
             (local.get $8)
            )
           )
           (local.get $6)
          )
         )
         (i32.store offset=4
          (local.get $4)
          (i32.or
           (i32.or
            (local.get $6)
            (i32.and
             (local.get $9)
             (i32.const 1)
            )
           )
           (i32.const 2)
          )
         )
         (i32.store offset=4
          (local.tee $3
           (i32.add
            (local.get $4)
            (local.get $6)
           )
          )
          (i32.or
           (local.tee $2
            (i32.sub
             (local.get $8)
             (local.get $6)
            )
           )
           (i32.const 1)
          )
         )
         (i32.store
          (i32.const 3284)
          (local.get $2)
         )
         (i32.store
          (i32.const 3296)
          (local.get $3)
         )
         (br $block2)
        )
       )
       (if
        (i32.eq
         (i32.load
          (i32.const 3292)
         )
         (local.get $7)
        )
        (then
         (br_if $block
          (i32.lt_u
           (local.tee $3
            (i32.add
             (i32.load
              (i32.const 3280)
             )
             (local.get $8)
            )
           )
           (local.get $6)
          )
         )
         (block $block3
          (if
           (i32.ge_u
            (local.tee $2
             (i32.sub
              (local.get $3)
              (local.get $6)
             )
            )
            (i32.const 16)
           )
           (then
            (i32.store offset=4
             (local.get $4)
             (i32.or
              (i32.or
               (local.get $6)
               (i32.and
                (local.get $9)
                (i32.const 1)
               )
              )
              (i32.const 2)
             )
            )
            (i32.store offset=4
             (local.tee $8
              (i32.add
               (local.get $4)
               (local.get $6)
              )
             )
             (i32.or
              (local.get $2)
              (i32.const 1)
             )
            )
            (i32.store
             (local.tee $3
              (i32.add
               (local.get $3)
               (local.get $4)
              )
             )
             (local.get $2)
            )
            (i32.store offset=4
             (local.get $3)
             (i32.and
              (i32.load offset=4
               (local.get $3)
              )
              (i32.const -2)
             )
            )
            (br $block3)
           )
          )
          (i32.store offset=4
           (local.get $4)
           (i32.or
            (i32.or
             (i32.and
              (local.get $9)
              (i32.const 1)
             )
             (local.get $3)
            )
            (i32.const 2)
           )
          )
          (i32.store offset=4
           (local.tee $2
            (i32.add
             (local.get $3)
             (local.get $4)
            )
           )
           (i32.or
            (i32.load offset=4
             (local.get $2)
            )
            (i32.const 1)
           )
          )
          (local.set $2
           (i32.const 0)
          )
          (local.set $8
           (i32.const 0)
          )
         )
         (i32.store
          (i32.const 3292)
          (local.get $8)
         )
         (i32.store
          (i32.const 3280)
          (local.get $2)
         )
         (br $block2)
        )
       )
       (br_if $block
        (i32.and
         (local.tee $3
          (i32.load offset=4
           (local.get $7)
          )
         )
         (i32.const 2)
        )
       )
       (br_if $block
        (i32.lt_u
         (local.tee $11
          (i32.add
           (i32.and
            (local.get $3)
            (i32.const -8)
           )
           (local.get $8)
          )
         )
         (local.get $6)
        )
       )
       (local.set $12
        (i32.sub
         (local.get $11)
         (local.get $6)
        )
       )
       (local.set $5
        (i32.load offset=12
         (local.get $7)
        )
       )
       (block $block4
        (if
         (i32.le_u
          (local.get $3)
          (i32.const 255)
         )
         (then
          (if
           (i32.eq
            (local.tee $2
             (i32.load offset=8
              (local.get $7)
             )
            )
            (local.get $5)
           )
           (then
            (i32.store
             (i32.const 3272)
             (i32.and
              (i32.load
               (i32.const 3272)
              )
              (i32.rotl
               (i32.const -2)
               (i32.shr_u
                (local.get $3)
                (i32.const 3)
               )
              )
             )
            )
            (br $block4)
           )
          )
          (i32.store offset=12
           (local.get $2)
           (local.get $5)
          )
          (i32.store offset=8
           (local.get $5)
           (local.get $2)
          )
          (br $block4)
         )
        )
        (local.set $10
         (i32.load offset=24
          (local.get $7)
         )
        )
        (block $block5
         (if
          (i32.ne
           (local.get $5)
           (local.get $7)
          )
          (then
           (i32.store offset=12
            (local.tee $2
             (i32.load offset=8
              (local.get $7)
             )
            )
            (local.get $5)
           )
           (i32.store offset=8
            (local.get $5)
            (local.get $2)
           )
           (br $block5)
          )
         )
         (block $block6
          (local.set $8
           (if (result i32)
            (local.tee $2
             (i32.load offset=20
              (local.get $7)
             )
            )
            (then
             (i32.add
              (local.get $7)
              (i32.const 20)
             )
            )
            (else
             (br_if $block6
              (i32.eqz
               (local.tee $2
                (i32.load offset=16
                 (local.get $7)
                )
               )
              )
             )
             (i32.add
              (local.get $7)
              (i32.const 16)
             )
            )
           )
          )
          (loop $label
           (local.set $3
            (local.get $8)
           )
           (local.set $8
            (i32.add
             (local.tee $5
              (local.get $2)
             )
             (i32.const 20)
            )
           )
           (br_if $label
            (local.tee $2
             (i32.load offset=20
              (local.get $2)
             )
            )
           )
           (local.set $8
            (i32.add
             (local.get $5)
             (i32.const 16)
            )
           )
           (br_if $label
            (local.tee $2
             (i32.load offset=16
              (local.get $5)
             )
            )
           )
          )
          (i32.store
           (local.get $3)
           (i32.const 0)
          )
          (br $block5)
         )
         (local.set $5
          (i32.const 0)
         )
        )
        (br_if $block4
         (i32.eqz
          (local.get $10)
         )
        )
        (block $block7
         (if
          (i32.eq
           (i32.load
            (local.tee $2
             (i32.add
              (i32.shl
               (local.tee $3
                (i32.load offset=28
                 (local.get $7)
                )
               )
               (i32.const 2)
              )
              (i32.const 3576)
             )
            )
           )
           (local.get $7)
          )
          (then
           (i32.store
            (local.get $2)
            (local.get $5)
           )
           (br_if $block7
            (local.get $5)
           )
           (i32.store
            (i32.const 3276)
            (i32.and
             (i32.load
              (i32.const 3276)
             )
             (i32.rotl
              (i32.const -2)
              (local.get $3)
             )
            )
           )
           (br $block4)
          )
         )
         (block $block8
          (if
           (i32.eq
            (local.get $7)
            (i32.load offset=16
             (local.get $10)
            )
           )
           (then
            (i32.store offset=16
             (local.get $10)
             (local.get $5)
            )
            (br $block8)
           )
          )
          (i32.store offset=20
           (local.get $10)
           (local.get $5)
          )
         )
         (br_if $block4
          (i32.eqz
           (local.get $5)
          )
         )
        )
        (i32.store offset=24
         (local.get $5)
         (local.get $10)
        )
        (if
         (local.tee $2
          (i32.load offset=16
           (local.get $7)
          )
         )
         (then
          (i32.store offset=16
           (local.get $5)
           (local.get $2)
          )
          (i32.store offset=24
           (local.get $2)
           (local.get $5)
          )
         )
        )
        (br_if $block4
         (i32.eqz
          (local.tee $2
           (i32.load offset=20
            (local.get $7)
           )
          )
         )
        )
        (i32.store offset=20
         (local.get $5)
         (local.get $2)
        )
        (i32.store offset=24
         (local.get $2)
         (local.get $5)
        )
       )
       (if
        (i32.le_u
         (local.get $12)
         (i32.const 15)
        )
        (then
         (i32.store offset=4
          (local.get $4)
          (i32.or
           (i32.or
            (i32.and
             (local.get $9)
             (i32.const 1)
            )
            (local.get $11)
           )
           (i32.const 2)
          )
         )
         (i32.store offset=4
          (local.tee $2
           (i32.add
            (local.get $4)
            (local.get $11)
           )
          )
          (i32.or
           (i32.load offset=4
            (local.get $2)
           )
           (i32.const 1)
          )
         )
         (br $block2)
        )
       )
       (i32.store offset=4
        (local.get $4)
        (i32.or
         (i32.or
          (local.get $6)
          (i32.and
           (local.get $9)
           (i32.const 1)
          )
         )
         (i32.const 2)
        )
       )
       (i32.store offset=4
        (local.tee $3
         (i32.add
          (local.get $4)
          (local.get $6)
         )
        )
        (i32.or
         (local.get $12)
         (i32.const 3)
        )
       )
       (i32.store offset=4
        (local.tee $2
         (i32.add
          (local.get $4)
          (local.get $11)
         )
        )
        (i32.or
         (i32.load offset=4
          (local.get $2)
         )
         (i32.const 1)
        )
       )
       (call $22
        (local.get $3)
        (local.get $12)
       )
      )
      (local.set $2
       (local.get $4)
      )
     )
     (local.get $2)
    )
   )
   (then
    (return
     (i32.add
      (local.get $2)
      (i32.const 8)
     )
    )
   )
  )
  (if
   (i32.eqz
    (local.tee $4
     (call $19
      (local.get $1)
     )
    )
   )
   (then
    (return
     (i32.const 0)
    )
   )
  )
  (call $8
   (local.get $4)
   (local.get $0)
   (select
    (local.tee $2
     (i32.add
      (select
       (i32.const -4)
       (i32.const -8)
       (i32.and
        (local.tee $2
         (i32.load
          (i32.sub
           (local.get $0)
           (i32.const 4)
          )
         )
        )
        (i32.const 3)
       )
      )
      (i32.and
       (local.get $2)
       (i32.const -8)
      )
     )
    )
    (local.get $1)
    (i32.gt_u
     (local.get $1)
     (local.get $2)
    )
   )
  )
  (call $20
   (local.get $0)
  )
  (local.get $4)
 )
 (func $22 (param $0 i32) (param $1 i32)
  (local $2 i32)
  (local $3 i32)
  (local $4 i32)
  (local $5 i32)
  (local $6 i32)
  (local $7 i32)
  (local $8 i32)
  (local.set $5
   (i32.add
    (local.get $0)
    (local.get $1)
   )
  )
  (block $block1
   (block $block
    (br_if $block
     (i32.and
      (local.tee $2
       (i32.load offset=4
        (local.get $0)
       )
      )
      (i32.const 1)
     )
    )
    (br_if $block1
     (i32.eqz
      (i32.and
       (local.get $2)
       (i32.const 2)
      )
     )
    )
    (local.set $1
     (i32.add
      (local.tee $2
       (i32.load
        (local.get $0)
       )
      )
      (local.get $1)
     )
    )
    (block $block3
     (block $block4
      (block $block2
       (if
        (i32.ne
         (local.tee $0
          (i32.sub
           (local.get $0)
           (local.get $2)
          )
         )
         (i32.load
          (i32.const 3292)
         )
        )
        (then
         (local.set $3
          (i32.load offset=12
           (local.get $0)
          )
         )
         (if
          (i32.le_u
           (local.get $2)
           (i32.const 255)
          )
          (then
           (br_if $block2
            (i32.ne
             (local.get $3)
             (local.tee $4
              (i32.load offset=8
               (local.get $0)
              )
             )
            )
           )
           (i32.store
            (i32.const 3272)
            (i32.and
             (i32.load
              (i32.const 3272)
             )
             (i32.rotl
              (i32.const -2)
              (i32.shr_u
               (local.get $2)
               (i32.const 3)
              )
             )
            )
           )
           (br $block)
          )
         )
         (local.set $6
          (i32.load offset=24
           (local.get $0)
          )
         )
         (if
          (i32.ne
           (local.get $0)
           (local.get $3)
          )
          (then
           (i32.store offset=12
            (local.tee $2
             (i32.load offset=8
              (local.get $0)
             )
            )
            (local.get $3)
           )
           (i32.store offset=8
            (local.get $3)
            (local.get $2)
           )
           (br $block3)
          )
         )
         (local.set $2
          (if (result i32)
           (local.tee $4
            (i32.load offset=20
             (local.get $0)
            )
           )
           (then
            (i32.add
             (local.get $0)
             (i32.const 20)
            )
           )
           (else
            (br_if $block4
             (i32.eqz
              (local.tee $4
               (i32.load offset=16
                (local.get $0)
               )
              )
             )
            )
            (i32.add
             (local.get $0)
             (i32.const 16)
            )
           )
          )
         )
         (loop $label
          (local.set $7
           (local.get $2)
          )
          (local.set $2
           (i32.add
            (local.tee $3
             (local.get $4)
            )
            (i32.const 20)
           )
          )
          (br_if $label
           (local.tee $4
            (i32.load offset=20
             (local.get $3)
            )
           )
          )
          (local.set $2
           (i32.add
            (local.get $3)
            (i32.const 16)
           )
          )
          (br_if $label
           (local.tee $4
            (i32.load offset=16
             (local.get $3)
            )
           )
          )
         )
         (i32.store
          (local.get $7)
          (i32.const 0)
         )
         (br $block3)
        )
       )
       (br_if $block
        (i32.ne
         (i32.and
          (local.tee $2
           (i32.load offset=4
            (local.get $5)
           )
          )
          (i32.const 3)
         )
         (i32.const 3)
        )
       )
       (i32.store
        (i32.const 3280)
        (local.get $1)
       )
       (i32.store offset=4
        (local.get $5)
        (i32.and
         (local.get $2)
         (i32.const -2)
        )
       )
       (i32.store offset=4
        (local.get $0)
        (i32.or
         (local.get $1)
         (i32.const 1)
        )
       )
       (i32.store
        (local.get $5)
        (local.get $1)
       )
       (return)
      )
      (i32.store offset=12
       (local.get $4)
       (local.get $3)
      )
      (i32.store offset=8
       (local.get $3)
       (local.get $4)
      )
      (br $block)
     )
     (local.set $3
      (i32.const 0)
     )
    )
    (br_if $block
     (i32.eqz
      (local.get $6)
     )
    )
    (block $block5
     (if
      (i32.eq
       (i32.load
        (local.tee $4
         (i32.add
          (i32.shl
           (local.tee $2
            (i32.load offset=28
             (local.get $0)
            )
           )
           (i32.const 2)
          )
          (i32.const 3576)
         )
        )
       )
       (local.get $0)
      )
      (then
       (i32.store
        (local.get $4)
        (local.get $3)
       )
       (br_if $block5
        (local.get $3)
       )
       (i32.store
        (i32.const 3276)
        (i32.and
         (i32.load
          (i32.const 3276)
         )
         (i32.rotl
          (i32.const -2)
          (local.get $2)
         )
        )
       )
       (br $block)
      )
     )
     (block $block6
      (if
       (i32.eq
        (local.get $0)
        (i32.load offset=16
         (local.get $6)
        )
       )
       (then
        (i32.store offset=16
         (local.get $6)
         (local.get $3)
        )
        (br $block6)
       )
      )
      (i32.store offset=20
       (local.get $6)
       (local.get $3)
      )
     )
     (br_if $block
      (i32.eqz
       (local.get $3)
      )
     )
    )
    (i32.store offset=24
     (local.get $3)
     (local.get $6)
    )
    (if
     (local.tee $2
      (i32.load offset=16
       (local.get $0)
      )
     )
     (then
      (i32.store offset=16
       (local.get $3)
       (local.get $2)
      )
      (i32.store offset=24
       (local.get $2)
       (local.get $3)
      )
     )
    )
    (br_if $block
     (i32.eqz
      (local.tee $2
       (i32.load offset=20
        (local.get $0)
       )
      )
     )
    )
    (i32.store offset=20
     (local.get $3)
     (local.get $2)
    )
    (i32.store offset=24
     (local.get $2)
     (local.get $3)
    )
   )
   (block $block10
    (block $block7
     (block $block8
      (block $block9
       (if
        (i32.eqz
         (i32.and
          (local.tee $2
           (i32.load offset=4
            (local.get $5)
           )
          )
          (i32.const 2)
         )
        )
        (then
         (if
          (i32.eq
           (i32.load
            (i32.const 3296)
           )
           (local.get $5)
          )
          (then
           (i32.store
            (i32.const 3296)
            (local.get $0)
           )
           (i32.store
            (i32.const 3284)
            (local.tee $1
             (i32.add
              (i32.load
               (i32.const 3284)
              )
              (local.get $1)
             )
            )
           )
           (i32.store offset=4
            (local.get $0)
            (i32.or
             (local.get $1)
             (i32.const 1)
            )
           )
           (br_if $block1
            (i32.ne
             (local.get $0)
             (i32.load
              (i32.const 3292)
             )
            )
           )
           (i32.store
            (i32.const 3280)
            (i32.const 0)
           )
           (i32.store
            (i32.const 3292)
            (i32.const 0)
           )
           (return)
          )
         )
         (if
          (i32.eq
           (local.tee $8
            (i32.load
             (i32.const 3292)
            )
           )
           (local.get $5)
          )
          (then
           (i32.store
            (i32.const 3292)
            (local.get $0)
           )
           (i32.store
            (i32.const 3280)
            (local.tee $1
             (i32.add
              (i32.load
               (i32.const 3280)
              )
              (local.get $1)
             )
            )
           )
           (i32.store offset=4
            (local.get $0)
            (i32.or
             (local.get $1)
             (i32.const 1)
            )
           )
           (i32.store
            (i32.add
             (local.get $0)
             (local.get $1)
            )
            (local.get $1)
           )
           (return)
          )
         )
         (local.set $1
          (i32.add
           (i32.and
            (local.get $2)
            (i32.const -8)
           )
           (local.get $1)
          )
         )
         (local.set $3
          (i32.load offset=12
           (local.get $5)
          )
         )
         (if
          (i32.le_u
           (local.get $2)
           (i32.const 255)
          )
          (then
           (if
            (i32.eq
             (local.tee $4
              (i32.load offset=8
               (local.get $5)
              )
             )
             (local.get $3)
            )
            (then
             (i32.store
              (i32.const 3272)
              (i32.and
               (i32.load
                (i32.const 3272)
               )
               (i32.rotl
                (i32.const -2)
                (i32.shr_u
                 (local.get $2)
                 (i32.const 3)
                )
               )
              )
             )
             (br $block7)
            )
           )
           (i32.store offset=12
            (local.get $4)
            (local.get $3)
           )
           (i32.store offset=8
            (local.get $3)
            (local.get $4)
           )
           (br $block7)
          )
         )
         (local.set $6
          (i32.load offset=24
           (local.get $5)
          )
         )
         (if
          (i32.ne
           (local.get $3)
           (local.get $5)
          )
          (then
           (i32.store offset=12
            (local.tee $2
             (i32.load offset=8
              (local.get $5)
             )
            )
            (local.get $3)
           )
           (i32.store offset=8
            (local.get $3)
            (local.get $2)
           )
           (br $block8)
          )
         )
         (local.set $2
          (if (result i32)
           (local.tee $4
            (i32.load offset=20
             (local.get $5)
            )
           )
           (then
            (i32.add
             (local.get $5)
             (i32.const 20)
            )
           )
           (else
            (br_if $block9
             (i32.eqz
              (local.tee $4
               (i32.load offset=16
                (local.get $5)
               )
              )
             )
            )
            (i32.add
             (local.get $5)
             (i32.const 16)
            )
           )
          )
         )
         (loop $label1
          (local.set $7
           (local.get $2)
          )
          (local.set $2
           (i32.add
            (local.tee $3
             (local.get $4)
            )
            (i32.const 20)
           )
          )
          (br_if $label1
           (local.tee $4
            (i32.load offset=20
             (local.get $3)
            )
           )
          )
          (local.set $2
           (i32.add
            (local.get $3)
            (i32.const 16)
           )
          )
          (br_if $label1
           (local.tee $4
            (i32.load offset=16
             (local.get $3)
            )
           )
          )
         )
         (i32.store
          (local.get $7)
          (i32.const 0)
         )
         (br $block8)
        )
       )
       (i32.store offset=4
        (local.get $5)
        (i32.and
         (local.get $2)
         (i32.const -2)
        )
       )
       (i32.store offset=4
        (local.get $0)
        (i32.or
         (local.get $1)
         (i32.const 1)
        )
       )
       (i32.store
        (i32.add
         (local.get $0)
         (local.get $1)
        )
        (local.get $1)
       )
       (br $block10)
      )
      (local.set $3
       (i32.const 0)
      )
     )
     (br_if $block7
      (i32.eqz
       (local.get $6)
      )
     )
     (block $block11
      (if
       (i32.eq
        (i32.load
         (local.tee $4
          (i32.add
           (i32.shl
            (local.tee $2
             (i32.load offset=28
              (local.get $5)
             )
            )
            (i32.const 2)
           )
           (i32.const 3576)
          )
         )
        )
        (local.get $5)
       )
       (then
        (i32.store
         (local.get $4)
         (local.get $3)
        )
        (br_if $block11
         (local.get $3)
        )
        (i32.store
         (i32.const 3276)
         (i32.and
          (i32.load
           (i32.const 3276)
          )
          (i32.rotl
           (i32.const -2)
           (local.get $2)
          )
         )
        )
        (br $block7)
       )
      )
      (block $block12
       (if
        (i32.eq
         (local.get $5)
         (i32.load offset=16
          (local.get $6)
         )
        )
        (then
         (i32.store offset=16
          (local.get $6)
          (local.get $3)
         )
         (br $block12)
        )
       )
       (i32.store offset=20
        (local.get $6)
        (local.get $3)
       )
      )
      (br_if $block7
       (i32.eqz
        (local.get $3)
       )
      )
     )
     (i32.store offset=24
      (local.get $3)
      (local.get $6)
     )
     (if
      (local.tee $2
       (i32.load offset=16
        (local.get $5)
       )
      )
      (then
       (i32.store offset=16
        (local.get $3)
        (local.get $2)
       )
       (i32.store offset=24
        (local.get $2)
        (local.get $3)
       )
      )
     )
     (br_if $block7
      (i32.eqz
       (local.tee $2
        (i32.load offset=20
         (local.get $5)
        )
       )
      )
     )
     (i32.store offset=20
      (local.get $3)
      (local.get $2)
     )
     (i32.store offset=24
      (local.get $2)
      (local.get $3)
     )
    )
    (i32.store offset=4
     (local.get $0)
     (i32.or
      (local.get $1)
      (i32.const 1)
     )
    )
    (i32.store
     (i32.add
      (local.get $0)
      (local.get $1)
     )
     (local.get $1)
    )
    (br_if $block10
     (i32.ne
      (local.get $0)
      (local.get $8)
     )
    )
    (i32.store
     (i32.const 3280)
     (local.get $1)
    )
    (return)
   )
   (if
    (i32.le_u
     (local.get $1)
     (i32.const 255)
    )
    (then
     (local.set $2
      (i32.add
       (i32.and
        (local.get $1)
        (i32.const -8)
       )
       (i32.const 3312)
      )
     )
     (local.set $1
      (block $block13 (result i32)
       (if
        (i32.eqz
         (i32.and
          (local.tee $3
           (i32.load
            (i32.const 3272)
           )
          )
          (local.tee $1
           (i32.shl
            (i32.const 1)
            (i32.shr_u
             (local.get $1)
             (i32.const 3)
            )
           )
          )
         )
        )
        (then
         (i32.store
          (i32.const 3272)
          (i32.or
           (local.get $1)
           (local.get $3)
          )
         )
         (br $block13
          (local.get $2)
         )
        )
       )
       (i32.load offset=8
        (local.get $2)
       )
      )
     )
     (i32.store offset=8
      (local.get $2)
      (local.get $0)
     )
     (i32.store offset=12
      (local.get $1)
      (local.get $0)
     )
     (i32.store offset=12
      (local.get $0)
      (local.get $2)
     )
     (i32.store offset=8
      (local.get $0)
      (local.get $1)
     )
     (return)
    )
   )
   (local.set $3
    (i32.const 31)
   )
   (if
    (i32.le_u
     (local.get $1)
     (i32.const 16777215)
    )
    (then
     (local.set $3
      (i32.add
       (i32.sub
        (i32.and
         (i32.shr_u
          (local.get $1)
          (i32.sub
           (i32.const 38)
           (local.tee $2
            (i32.clz
             (i32.shr_u
              (local.get $1)
              (i32.const 8)
             )
            )
           )
          )
         )
         (i32.const 1)
        )
        (i32.shl
         (local.get $2)
         (i32.const 1)
        )
       )
       (i32.const 62)
      )
     )
    )
   )
   (i32.store offset=28
    (local.get $0)
    (local.get $3)
   )
   (i64.store offset=16 align=4
    (local.get $0)
    (i64.const 0)
   )
   (local.set $2
    (i32.add
     (i32.shl
      (local.get $3)
      (i32.const 2)
     )
     (i32.const 3576)
    )
   )
   (block $block15
    (block $block14
     (if
      (i32.eqz
       (i32.and
        (local.tee $4
         (i32.load
          (i32.const 3276)
         )
        )
        (local.tee $7
         (i32.shl
          (i32.const 1)
          (local.get $3)
         )
        )
       )
      )
      (then
       (i32.store
        (i32.const 3276)
        (i32.or
         (local.get $4)
         (local.get $7)
        )
       )
       (i32.store
        (local.get $2)
        (local.get $0)
       )
       (i32.store offset=24
        (local.get $0)
        (local.get $2)
       )
       (br $block14)
      )
     )
     (local.set $3
      (i32.shl
       (local.get $1)
       (select
        (i32.sub
         (i32.const 25)
         (i32.shr_u
          (local.get $3)
          (i32.const 1)
         )
        )
        (i32.const 0)
        (i32.ne
         (local.get $3)
         (i32.const 31)
        )
       )
      )
     )
     (local.set $2
      (i32.load
       (local.get $2)
      )
     )
     (loop $label2
      (br_if $block15
       (i32.eq
        (i32.and
         (i32.load offset=4
          (local.tee $4
           (local.get $2)
          )
         )
         (i32.const -8)
        )
        (local.get $1)
       )
      )
      (local.set $2
       (i32.shr_u
        (local.get $3)
        (i32.const 29)
       )
      )
      (local.set $3
       (i32.shl
        (local.get $3)
        (i32.const 1)
       )
      )
      (br_if $label2
       (local.tee $2
        (i32.load offset=16
         (local.tee $7
          (i32.add
           (local.get $4)
           (i32.and
            (local.get $2)
            (i32.const 4)
           )
          )
         )
        )
       )
      )
     )
     (i32.store offset=16
      (local.get $7)
      (local.get $0)
     )
     (i32.store offset=24
      (local.get $0)
      (local.get $4)
     )
    )
    (i32.store offset=12
     (local.get $0)
     (local.get $0)
    )
    (i32.store offset=8
     (local.get $0)
     (local.get $0)
    )
    (return)
   )
   (i32.store offset=12
    (local.tee $1
     (i32.load offset=8
      (local.get $4)
     )
    )
    (local.get $0)
   )
   (i32.store offset=8
    (local.get $4)
    (local.get $0)
   )
   (i32.store offset=24
    (local.get $0)
    (i32.const 0)
   )
   (i32.store offset=12
    (local.get $0)
    (local.get $4)
   )
   (i32.store offset=8
    (local.get $0)
    (local.get $1)
   )
  )
 )
 (func $23 (param $0 i32) (result i32)
  (local $1 i32)
  (local $2 i32)
  (local.set $0
   (i32.add
    (local.tee $1
     (i32.load
      (i32.const 2028)
     )
    )
    (local.tee $2
     (i32.and
      (i32.add
       (local.get $0)
       (i32.const 7)
      )
      (i32.const -8)
     )
    )
   )
  )
  (block $block
   (if
    (i32.eqz
     (select
      (local.get $2)
      (i32.const 0)
      (i32.le_u
       (local.get $0)
       (local.get $1)
      )
     )
    )
    (then
     (br_if $block
      (i32.le_u
       (local.get $0)
       (i32.shl
        (memory.size)
        (i32.const 16)
       )
      )
     )
    )
   )
   (i32.store
    (i32.const 2220)
    (i32.const 48)
   )
   (return
    (i32.const -1)
   )
  )
  (i32.store
   (i32.const 2028)
   (local.get $0)
  )
  (local.get $1)
 )
 (func $24 (param $0 i32)
  (global.set $global$0
   (local.get $0)
  )
 )
 (func $25 (result i32)
  (global.get $global$0)
 )
)

