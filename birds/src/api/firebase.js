const firebase = {
  addUser: function addUser(firebase, { email, password }) {
    return firebase.auth().createUserWithEmailAndPassword(email, password);
  },
  getUser: function getUser(firebase) {
    return Promise.resolve(firebase.auth().currentUser);
  },
};


export default firebase;
