process.env.NODE_ENV = 'test';

const request = require('supertest');
const app = require("../app.js");

describe('POST /api/v1/users', () => {
  it('respond with json', (done) => {
    request(app)
      .post('/api/v1/users')
      .set('Accept', 'application/json')
      .send({email:'test@team4159.org', password: 'password', firstname: 'Test', lastname: 'Account', teamnumber: 4159})
      .expect('Content-Type', /json/)
      .expect(200, done);
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
        .expect('Content-Type', /json/)
        .expect(200, done);
  });
});
