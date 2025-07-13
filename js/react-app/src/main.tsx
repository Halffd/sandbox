import React, { useState } from 'react';
import ReactDOM from 'react-dom';

const App = () => {
    const [likes, setLikes] = useState(0);
    const [likeDates, setLikeDates] = useState([]);

    const likeAndSubscribe = () => {
        setLikes(likes + 1);
        // Add the current date to the log
        const currentDate = new Date().toLocaleString();
        setLikeDates([...likeDates, currentDate]);
    };

    return (
        <div>
            <Header onLike={likeAndSubscribe} />
            <h2>Likes: {likes}</h2>
            <LikeLog dates={likeDates} />
        </div>
    );
};

const Header = ({ onLike }) => {
    return (
        <header>
            <Logo icon="👍" />
            <Title text="React in 100 Seconds" />
            <Action onClick={onLike} />
        </header>
    );
};

const Logo = ({ icon }) => <div>{icon}</div>;

const Title = ({ text }) => <h1>{text}</h1>;

const Action = ({ onClick }) => (
    <button onClick={onClick}>Like</button>
);

const LikeLog = ({ dates }) => {
    return (
        <div>
            <h3>Like Log:</h3>
            <ul>
                {dates.map((date, index) => (
                    <li key={index}>{date}</li>
                ))}
            </ul>
        </div>
    );
};

// Render the App component
ReactDOM.render(<App />, document.getElementById('root'));
