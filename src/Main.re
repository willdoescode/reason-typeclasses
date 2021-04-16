module type EQ = {
  type t;
  let equals: (t, t) => bool;
};

module StringEq: EQ with type t = string = {
  type t = string;
  let equals = (a: string, b: string) => a == b;
};

module IntEq: EQ with type t = int = {
  type t = int;
  let equals = (a: int, b: int) => a == b;
};

module type ORD = {
  include EQ;
  let lte: (t, t) => bool;
};

module IntOrd: ORD with type t = int = {
  include IntEq;
  let lte = (a: int, b: int) => a <= b;
};

module OrdExtras = (Ord: ORD) => {
  let lt = (a, b) => Ord.lte(a, b) && !Ord.equals(a, b);
  let gt = (a, b) => !Ord.lte(a, b);
  let gte = (a, b) => gt(a, b) || Ord.equals(a, b);

  let inRange = (~min: Ord.t, ~max: Ord.t, value: Ord.t): bool =>
    gte(value, min) && Ord.lte(value, max);

  let clamp = (~min: Ord.t, ~max: Ord.t, value: Ord.t): Ord.t =>
    if (lt(value, min)) {
      min;
    } else if (gt(value, max)) {
      max;
    } else {
      value;
    };
};

module IntOrdExtras = OrdExtras(IntOrd);

let _ = IntOrdExtras.clamp(~min=4, ~max=12, 1);

type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December;

let toInt = month =>
  switch (month) {
  | Jan => 0
  | Feb => 1
  | Mar => 2
  | Apr => 3
  | May => 4
  | June => 5
  | July => 6
  | August => 7
  | September => 8
  | October => 9
  | November => 10
  | December => 11
  };

module MonthOrd: ORD with type t = month = {
  type t = month;
  let equals = (a: month, b: month) => a == b;
  let lte = (a, b) => IntOrd.lte(toInt(a), toInt(b));
};

module MonthOrdExtras = OrdExtras(MonthOrd);

let _ = MonthOrdExtras.inRange(~min=Feb, ~max=Apr, Mar);
