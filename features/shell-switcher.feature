Feature: Shell-switcher works
  In order to do switch between shell buffers
  As a user
  I want have easy-to-use keystrokes

  Background:
    Given I setup shell-switcher-mode
    And I setup a mock shell function
    And I kill all shell-switcher buffers
    And I setup y-or-n-p to always answer t

  Scenario: C-' opens the first 2 buffers (one after the other) and switch between them
    When I am in buffer "*scratch*"
    And I press "C-'"
    Then I should be in buffer "ecukes-shell"

    When I press "C-'"
    Then I should be in buffer "ecukes-shell<2>"

    When I press "C-'"
    Then I should be in buffer "ecukes-shell"

    When I press "C-'"
    Then I should be in buffer "ecukes-shell<2>"

  Scenario: Repeating ' continues switching after an initial C-'
    When I am in buffer "*scratch*"
    And I press "C-'"
    Then I should be in buffer "ecukes-shell"

    When I press "C-'"
    Then I should be in buffer "ecukes-shell<2>"

    When I press "C-'"
    Then I should be in buffer "ecukes-shell"

    When I press "'"
    Then I should be in buffer "ecukes-shell<2>"

    When I press "'"
    Then I should be in buffer "ecukes-shell"

    When I press "'"
    Then I should be in buffer "ecukes-shell<2>"

  Scenario: Repeating ' continues switching after an initial C-'
    When I am in buffer "*scratch*"
    And I press "C-M-'"
    Then I should be in buffer "ecukes-shell"

    When I press "C-M-'"
    Then I should be in buffer "ecukes-shell<2>"

    When I press "C-M-'"
    Then I should be in buffer "ecukes-shell<3>"

    When I press "C-M-'"
    Then I should be in buffer "ecukes-shell<4>"
