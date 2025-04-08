# 📝 History Kanban Board

## 🟡 To Do

- [x] Make sure that the names used in the *history* match those the user sees at the UI level.
- [x] Write an **argument class** for each *type* that can be stored in the *history*.
- [ ] Write a **validate** function for each argument class.
      These functions return an instance of *Error* if something is not OK.
- [ ] Write an **eval** function for each *type*, which returns the desired result.
- [ ] Refactor the server functions:
  - Use the new **argument classes**
  - Use **validate** and **eval** functions
  - If validation fails, communicate it using `print_err`
- [ ] Write an `eval_history()` function:
  - [ ] Parse the JSON string to an R list.
  - [ ] Iterate over the list and for each step:
    - [ ] Create the appropriate argument class
    - [ ] Call its `validate()` function
    - [ ] Call its `eval()` function if validation passes
  - [ ] If an error is encountered, communicate the error and the step using `print_err`

