attacks (x1, y1) (x2, y2) = x1 == x2 || y1 == y2 || abs(x1 - x2) == abs(y1 - y2)

queens list =
  let pairs = list `zip` [1 ..]  -- convert to list of (x, y) pairs, e.g. [(1, 2), ... (8, 4)]
  in not (or [attacks p q | p <- pairs, q <- pairs, p /= q])