ALTER TABLE confirmation ADD COLUMN email TEXT;

UPDATE confirmation
SET email = (
    SELECT email 
    FROM user 
    WHERE user.id = confirmation.user_id
);

ALTER TABLE person ADD COLUMN confirmation_id INTEGER;

UPDATE person
SET confirmation_id = (
    SELECT id
    FROM confirmation
    WHERE confirmation.user_id = person.user_id
);
