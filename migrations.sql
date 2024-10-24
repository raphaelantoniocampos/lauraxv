ALTER TABLE confirmation ADD COLUMN email TEXT;

UPDATE confirmation
SET email = (
    SELECT email 
    FROM user 
    WHERE user.id = confirmation.user_id
);


