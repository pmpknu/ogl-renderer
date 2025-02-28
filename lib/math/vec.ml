module type Vec = sig
  type t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> float -> t
  val div : t -> float -> t
  val dot : t -> t -> float
  val length : t -> float
  val normalize : t -> t
end

module Vec2 : Vec with type t = float * float = struct
  type t = float * float
  let zero = (0.0, 0.0)
  let add (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
  let sub (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
  let mul (x, y) s = (x *. s, y *. s)
  let div (x, y) s = (x /. s, y /. s)
  let dot (x1, y1) (x2, y2) = (x1 *. x2) +. (y1 *. y2)
  let length (x, y) = sqrt ((x *. x) +. (y *. y))
  let normalize v = let len = length v in if len = 0.0 then zero else div v len
end

module Vec3 : Vec with type t = float * float * float = struct
  type t = float * float * float
  let zero = (0.0, 0.0, 0.0)
  let add (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)
  let sub (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)
  let mul (x, y, z) s = (x *. s, y *. s, z *. s)
  let div (x, y, z) s = (x /. s, y /. s, z /. s)
  let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)
  let length (x, y, z) = sqrt ((x *. x) +. (y *. y) +. (z *. z))
  let normalize v = let len = length v in if len = 0.0 then zero else div v len
end

module Vec4 : Vec with type t = float * float * float * float = struct
  type t = float * float * float * float
  let zero = (0.0, 0.0, 0.0, 0.0)
  let add (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1 +. x2, y1 +. y2, z1 +. z2, w1 +. w2)
  let sub (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1 -. x2, y1 -. y2, z1 -. z2, w1 -. w2)
  let mul (x, y, z, w) s = (x *. s, y *. s, z *. s, w *. s)
  let div (x, y, z, w) s = (x /. s, y /. s, z /. s, w /. s)
  let dot (x1, y1, z1, w1) (x2, y2, z2, w2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2) +. (w1 *. w2)
  let length (x, y, z, w) = sqrt ((x *. x) +. (y *. y) +. (z *. z) +. (w *. w))
  let normalize v = let len = length v in if len = 0.0 then zero else div v len
end
