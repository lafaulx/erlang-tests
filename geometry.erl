-module(geometry).
-export ([area/1]).

area({rectangle, W, H}) -> W * H;
area({circle, R}) -> 3.14 * R * R;
area({triangle, A, B, C}) when A*A + B*B =:= C*C -> A * B / 2.