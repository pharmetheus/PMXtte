
# Test 1: Basic functionality with sequential, sorted input
test_that("Converts simple sequential input correctly", {
  input_data <- c(1, 2, 3, 4)
  result <- factor_rtte(input_data)

  # Check if the output is a factor
  expect_s3_class(result, "factor")

  # Check the factor levels
  expect_equal(levels(result), c("Event #1", "Event #2", "Event #3", "Event #4"))

  # Check the factor values (should map 1->Event #1, 2->Event #2, etc.)
  expected_values <- factor(c("Event #1", "Event #2", "Event #3", "Event #4"))
  expect_equal(result, expected_values)
})

# Test 2: Handles unsorted input with duplicates, maintaining correct order
test_that("Handles unsorted input and duplicates correctly", {
  input_data <- c(3, 1, 2, 1, 3, 2, 4)
  result <- factor_rtte(input_data)

  # The unique sorted levels should be 1, 2, 3, 4
  expected_levels_vector <- paste0("Event #", c(1, 2, 3, 4))
  expect_equal(levels(result), expected_levels_vector)

  # Check the mapping: 3->Event #3, 1->Event #1, 2->Event #2, etc.
  expected_values <- factor(
    c("Event #3", "Event #1", "Event #2", "Event #1", "Event #3", "Event #2", "Event #4"),
    levels = expected_levels_vector
  )
  expect_equal(result, expected_values)
})

# Test 3: Handles non-sequential integer inputs
test_that("Works with sparse/non-sequential event numbers", {
  input_data <- c(10, 5, 20, 5, 10)
  result <- factor_rtte(input_data)

  # Unique sorted levels are 5, 10, 20
  expected_levels_vector <- paste0("Event #", c(5, 10, 20))
  expect_equal(levels(result), expected_levels_vector)

  # Check the values
  expected_values <- factor(
    c("Event #10", "Event #5", "Event #20", "Event #5", "Event #10"),
    levels = expected_levels_vector
  )
  expect_equal(result, expected_values)
})

# Test 4: Handles missing values (NA)
test_that("Preserves NA values in the output factor", {
  input_data <- c(1, 3, NA, 2, 1)
  result <- factor_rtte(input_data)

  # Check that the number of elements is preserved
  expect_length(result, 5)

  # Check that the NA value remains (the third element)
  expect_true(is.na(result[3]))

  # Check levels (NA is ignored in unique(sort(x)))
  expected_levels_vector <- paste0("Event #", c(1, 2, 3))
  expect_equal(levels(result), expected_levels_vector)

  # Check all values
  expected_values <- factor(
    c("Event #1", "Event #3", NA, "Event #2", "Event #1"),
    levels = expected_levels_vector
  )
  expect_equal(result, expected_values)
})

# Test 5: Handles zero and negative values (based on implementation)
test_that("Handles zero and negative values correctly", {
  input_data <- c(-5, 0, 1, -5)
  result <- factor_rtte(input_data)

  # Unique sorted levels are -5, 0, 1
  expected_levels_vector <- paste0("Event #", c(-5, 0, 1))
  expect_equal(levels(result), expected_levels_vector)

  # Check the values
  expected_values <- factor(
    c("Event #-5", "Event #0", "Event #1", "Event #-5"),
    levels = expected_levels_vector
  )
  expect_equal(result, expected_values)
})

# Test 6: Handles empty input
test_that("Returns an empty factor for empty input", {
  input_data <- numeric(0)
  result <- factor_rtte(input_data)

  expect_s3_class(result, "factor")
  expect_length(result, 0)
  expect_equal(levels(result), character(0))
})
