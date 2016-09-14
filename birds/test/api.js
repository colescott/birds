const randomstring = require("randomstring");
const request = require('supertest');
const app = require("../app.js");

const util = require("./util.js");

const testUser = {email:'test@team4159.org', password: 'password', firstname: 'Test', lastname: 'Account', teamnumber: 4159};
const testUserNoPass = {email: testUser.email, firstname: testUser.firstname, lastname: testUser.lastname, teamnumber: testUser.teamnumber};;

var testUserWithId;
var loginToken;

describe('APIv1'), () => {
    describe('POST /api/v1/users', () => {
        it('respond with json', (done) => {
            request(app)
            .post('/api/v1/users')
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(200, done);
        });
        it('created and returned new user', (done) => {
            request(app)
            .post('/api/v1/users')
            .set('Accept', 'application/json')
            .send(testUser)
            .expect('Content-Type', /json/)
            .expect(function(res) {
                if(res.body.name)
                    throw new Error('User already exists in database');
                delete res.body.data.user.id;
            })
            .expect(200, {
                data: {
                    user: testUserNoPass
                }
            }, done);
        });
        it('responds error on empty user', (done) => {
            request(app)
            .post('/api/v1/users')
            .set('Accept', 'application/json')
            .send({})
            .expect(function(res) {
                if(!(res.body.error.message == 'Email value required.'))
                    throw new Error('Server not sending error with "Email value required."')
            })
            .end(done);
        });
    });

    describe('GET /api/v1/users', () => {
        it('respond with json', (done) => {
            request(app)
            .get('/api/v1/users')
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(200, done);
        });
        it('conatins test user', (done) => {
            request(app)
            .get('/api/v1/users')
            .set('Accept', 'application/json')
            .expect(function(res) {
                var found = false;
                res.body.data.users.forEach( (user) => {
                    if(user.email == testUser.email)
                    {
                        testUserWithId = util.clone(user);
                        found = true;
                    }
                });
                if(!found)
                    throw new Error('user does not exist!');
            })
            .end(done);
        });
    });

    describe('POST /api/v1/auth/login', () => {
        it('respond with json', (done) => {
            request(app)
            .post('/api/v1/auth/login')
            .set('Accept', 'application/json')
            .send(testUser)
            .expect('Content-Type', /json/)
            .expect(200, done);
        });
        it('responds with user', (done) => {
            request(app)
            .post('/api/v1/auth/login')
            .set('Accept', 'application/json')
            .send(testUser)
            .expect(function(res) {
                if(!util.equal(res.body.data.user, testUserWithId))
                    throw new Error('user not in response')
            })
            .end(done);
        });
        it('responds with token', (done) => {
            request(app)
            .post('/api/v1/auth/login')
            .set('Accept', 'application/json')
            .send(testUser)
            .expect(function(res) {
                if(!res.body.data.token)
                    throw new Error('Login token not set')
                loginToken = res.body.data.token;
            })
            .end(done);
        });
        it('responds error on no email', (done) => {
            request(app)
            .post('/api/v1/auth/login')
            .set('Accept', 'application/json')
            .send({password: testUser.password})
            .expect(function(res) {
                if(!(res.body.error.message == 'Unauthorized'))
                    throw new Error('Server not sending error with "Unauthorized"')
            })
            .end(done);
        });
        it('responds error on no password', (done) => {
            request(app)
            .post('/api/v1/auth/login')
            .set('Accept', 'application/json')
            .send({email: testUser.email})
            .expect(function(res) {
                if(!(res.body.error.message == 'Unauthorized'))
                    throw new Error('Server not sending error with "Unauthorized"')
            })
            .end(done);
        });
        it('responds error on bad password', (done) => {
            request(app)
            .post('/api/v1/auth/login')
            .set('Accept', 'application/json')
            .send({email: testUser.email, password: 'notthepassword'})
            .expect(function(res) {
                if(!(res.body.error.message == 'Unauthorized'))
                    throw new Error('Server not sending error with "Unauthorized"')
            })
            .end(done);
        });
    });

    describe('GET /users/:id', () => {
        it('respond with json', (done) => {
            request(app)
            .get('/api/v1/users/' + testUserWithId.id)
            .set('Accept', 'application/json')
            .expect('Content-Type', /json/)
            .expect(200, done);
        });
        it('responds with user', (done) => {
            request(app)
            .get('/api/v1/users/' + testUserWithId.id)
            .set('Accept', 'application/json')
            .expect(function(res) {
                if(!util.equal(res.body.data.user, testUserWithId))
                    throw new Error('User not equal to test user')
            })
            .end(done);
        });
    });

    describe('PUT /users/:id', () => {
        it('respond with json', (done) => {
            request(app)
            .put('/api/v1/users/' + testUserWithId.id)
            .set('Accept', 'application/json')
            .set('Authorization', 'Bearer ' + loginToken)
            .send({})
            .expect('Content-Type', /json/)
            .expect(200, done);
        });
        it('responds with changed user', (done) => {
            const newUser = {
                firstname: randomstring.generate(),
                lastname: randomstring.generate(),
                teamnumber: util.randomTeamNumber(1, 6237)
            };
            request(app)
            .put('/api/v1/users/' + testUserWithId.id)
            .set('Accept', 'application/json')
            .set('Authorization', 'Bearer ' + loginToken)
            .send(newUser)
            .expect(function(res) {
                if(util.equal(res.body.data.user, testUserWithId))
                    throw new Error('User was not changed, equal to old user')
                if(res.body.data.user.firstname != newUser.firstname)
                    throw new Error('First name was not changed')
                if(res.body.data.user.lastname != newUser.lastname)
                    throw new Error('Last name was not changed')
                if(res.body.data.user.teamnumber != newUser.teamnumber)
                    throw new Error('Team number was not changed')
            })
            .end(done);
        });
        it('responds error on no token', (done) => {
            request(app)
            .put('/api/v1/users/' + testUserWithId.id)
            .set('Accept', 'application/json')
            .send({})
            .expect(function(res) {
                if(!(res.body.error.message == 'You need to be logged in to do this.'))
                    throw new Error('Server not sending error with "You need to be logged in to do this."')
            })
            .end(done);
        });
        it('responds error on bad token', (done) => {
            request(app)
            .put('/api/v1/users/' + testUserWithId.id)
            .set('Accept', 'application/json')
            .set('Authorization', 'Bearer correcthorsebatterystaple')
            .send({})
            .expect(function(res) {
                if(!(res.body.error.message == 'Invalid token.'))
                    throw new Error('Server not sending error with "Invalid token."')
            })
            .end(done);
        });
    });

    describe('PUT /users/:id/:delete', () => {
        it('responds user deleted', (done) => {
            request(app)
            .put('/api/v1/users/' + testUserWithId.id + '/delete')
            .set('Accept', 'application/json')
            .set('Authorization', 'Bearer ' + loginToken)
            .expect({
                data: {
                    message: "successfully deleted user."
                }
            }, done);
        });
        it('user is deleted from database', (done) => {
            request(app)
            .get('/api/v1/users/')
            .set('Accept', 'application/json')
            .expect(function(res) {
                var found = false;
                res.body.data.users.forEach( (user) => {
                    if(user.email == testUser.email)
                    {
                        testUserWithId = util.clone(user);
                        found = true;
                    }
                });
                if(found)
                    throw new Error('The user still exists');
            })
            .end(done);
        });
        it('responds error on no token', (done) => {
            request(app)
            .put('/api/v1/users/' + testUserWithId.id + '/delete')
            .set('Accept', 'application/json')
            .send({})
            .expect(function(res) {
                if(!(res.body.error.message == 'You need to be logged in to do this.'))
                    throw new Error('Server not sending error with "You need to be logged in to do this."')
            })
            .end(done);
        });
        it('responds error on bad token', (done) => {
            request(app)
            .put('/api/v1/users/' + testUserWithId.id + '/delete')
            .set('Accept', 'application/json')
            .set('Authorization', 'Bearer correcthorsebatterystaple')
            .send({})
            .expect(function(res) {
                if(!(res.body.error.message == 'Invalid token.'))
                    throw new Error('Server not sending error with "Invalid token."')
            })
            .end(done);
        });
    });
});
