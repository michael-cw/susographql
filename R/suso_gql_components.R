# Support functions, which are not exported

# Query block for calender event
calenderEvent<-'
  calendarEvent {
                 creatorUserId
                 interviewId
                 interviewKey
                 isCompleted
                 publicKey
                 startTimezone
                 startUtc
                 updateDateUtc
               }
  '
# query block for options
options<- '
  options {
          parentValue
          title
          value
        }
  '

# query block for entity
entity<- sprintf('
                   entity {
                            identifying
                            label
                            questionText
                            variable
                            variableType
                            scope
                            type
                            %s
                          }

                   ', options)
