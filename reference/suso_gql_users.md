# Get user information

Allows the user to retrieve filtered or unfiltered user data.

## Usage

``` r
suso_gql_users(
  endpoint = NULL,
  user = NULL,
  password = NULL,
  token = NULL,
  userName = NULL,
  fullName = NULL,
  isArchived = NULL,
  isLocked = NULL,
  creationDate = NULL,
  email = NULL,
  phoneNumber = NULL,
  id = NULL,
  role = NULL,
  sortby_userName = NULL,
  sortby_role = NULL,
  sortby_creationDate = NULL,
  take = NULL,
  skip = NULL
)
```

## Arguments

- endpoint:

  GraphQL endpoint of your server

- user:

  your API username

- password:

  API password

- token:

  If Survey Solutions server token is provided *apiUser* and *apiPass*
  will be ignored

- userName:

  specific user name

- fullName:

  fullName

- isArchived:

  isArchived

- isLocked:

  isLocked

- creationDate:

  user creation date

- email:

  user email

- phoneNumber:

  user phoneNumber

- id:

  user id

- role:

  user role

- sortby_userName:

  sort users by user name, either ASC for ascending or DESC for
  descending

- sortby_role:

  sort users by role, either ASC for ascending or DESC for descending

- sortby_creationDate:

  sort users by utc creation date, either ASC for ascending or DESC for
  descending

- take:

  take the specified integer numeber of maps

- skip:

  skip the first integer number of maps

## Value

if successfull, returns a list with the (filtered) responses

## Details

ATTENTION: This requires admin credentials, regular API credentials
won't work

## Examples

``` r
if (FALSE) { # suso_gql_pwcheck() == 200
## Requires Survey Solutions Server and ADMIN credentials!!

# Get all users without filter

suso_gql_users(endpoint = ep, user = adminuser,
password = adminpass, sortby_userName = "ASC")

# Get only supervisors

suso_gql_users(endpoint = ep, user = adminuser,
password = adminuser, , sortby_userName = "ASC",
role = "SUPERVISOR")
}
```
