#include <vector>

#include <Rcpp.h>

// https://homepages.inf.ed.ac.uk/rbf/HIPR2/thin.htm

struct filters_thin {
    const std::vector <bool> fa1 = {false, false, false, true,
        true, true, true};
    const std::vector <int> xa1 = {-1, 0, 1, 0, -1, 0, 1},
          ya1 = {1, 1, 1, 0, -1, -1, -1};

    const std::vector <bool> fa2 = {true, false, true, true,
        false, true, false};
    const std::vector <int> xa2 = {-1, 1, -1, 0, 1, -1, 1},
          ya2 = {1, 1, 0, 0, 0, -1, -1};

    const std::vector <bool> fa3 = {true, true, true, true,
        false, false, false};
    const std::vector <int> xa3 = {-1, 0, 1, 0, -1, 0, 1},
          ya3 = {1, 1, 1, 0, -1, -1, -1};

    const std::vector <bool> fa4 = {false, true, false, true,
        true, false, true};
    const std::vector <int> xa4 = {-1, 1, -1, 0, 1, -1, 1},
          ya4 = {1, 1, 0, 0, 0, -1, -1};

    const std::vector <bool> fb1 = {false, false, true, true, false, true};
    const std::vector <int> xb1 = {0, 1, -1, 0, 1, 0},
          vb2 = {1, 1, 0, 0, 0, -1};

    const std::vector <bool> fb2 = {true, true, true, false, false, false};
    const std::vector <int> xb2 = {1, -1, 0, 1, 0, 1},
          yb2 = {1, 0, 0, 0, -1, -1};

    const std::vector <bool> fb3 = {true, false, true, true, false, false};
    const std::vector <int> xb3 = {0, -1, 0, 1, -1, 1},
          yb3 = {1, 0, 0, 0, -1, -1};

    const std::vector <bool> fb4 = {false, false, false, true, true, true};
    const std::vector <int> xb4 = {-1, 0, -1, 0, 1, 0},
          yb4 = {1, 1, 0, 0, 0, -1};
};

struct filters_prune {
    const std::vector <bool> fa1 = {false, false, false, false,
        true, false, false};
    const std::vector <int> xa1 = {-1, 0, 1, -1, 0, 1, -1},
          ya1 = {1, 1, 1, 0, 0, 0, -1};

    const std::vector <bool> fa2 = {false, false, false, true,
        false, false, false};
    const std::vector <int> xa2 = {-1, 0, 1, 0, 1, 0, 1},
          ya2 = {1, 1, 1, 0, 0, -1, -1};

    const std::vector <bool> fa3 = {false, false, true, false,
        false, false, false};
    const std::vector <int> xa3 = {1, -1, 0, 1, -1, 0, 1},
          ya3 = {1, 0, 0, 0, -1, -1, -1};

    const std::vector <bool> fa4 = {false, false, false, true,
        false, false, false};
    const std::vector <int> xa4 = {-1, 0, -1, 0, -1, 0, 1},
          ya4 = {1, 1, 0, 0, -1, -1, -1};

    const std::vector <bool> fb1 = {false, false, false, false,
        true, false, false};
    const std::vector <int> xb1 = {-1, 0, 1, -1, 0, 1, 1},
          yb1 = {1, 1, 1, 0, 0, 0, -1};

    const std::vector <bool> fb2 = {false, false, true, false,
        false, false, false};
    const std::vector <int> xb2 = {0, 1, 0, 1, -1, 0, 1},
          yb2 = {1, 1, 0, 0, -1, -1, -1};

    const std::vector <bool> fb3 = {false, false, true, false,
        false, false, false};
    const std::vector <int> xb3 = {-1, -1, 0, 1, -1, 0, 1},
          yb3 = {1, 0, 0, 0, -1, -1, -1};

    const std::vector <bool> fb4 = {false, false, false, false,
        true, false, false};
    const std::vector <int> xb4 = {-1, 0, 1, -1, 0, -1, 0},
          yb4 = {1, 1, 1, 0, 0, -1, -1};
};

Rcpp::NumericVector rcpp_edge_thin (Rcpp::IntegerMatrix image);
